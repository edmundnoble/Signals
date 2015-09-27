package io.rxpebble

import io.rxpebble.Lexer._

object Output {
  implicit class WithMkStringMap[A](val ts: TraversableOnce[A]) extends AnyVal {
    def mkStringMap[B](delim: String)(f: A => B): String = ts.map(f).mkString(delim)
  }

  def runAndAppend[T](param: T, delim: String)(funs: (T => String)*): String = funs.mkStringMap(delim)(_(param))

  def generateForwardTypeDeclaration(typeName: Lexer.Type, structFields: Seq[StructField]): String = {
    s"struct $typeName;\ntypedef struct $typeName $typeName;"
  }

  def declareStructs(understood: Understood): String = {
    understood.structDefinitions.mkStringMap("\n")((generateForwardTypeDeclaration _).tupled)
  }

  def generateAlias(typeName: Lexer.Type, aliasTo: Lexer.Type): String = {
    s"typedef $typeName $aliasTo;"
  }

  def declareAliases(understood: Understood): String = {
    understood.typeAliases.mkStringMap("\n")((generateAlias _).tupled)
  }

  def generateDefinition(typeName: Lexer.Type, structFields: Seq[StructField]): String = {
    s"typedef struct $typeName {\n" +
      s"${
        structFields.headOption.fold("")({ head =>
          val tail = structFields.tail
          tail.foldLeft[String](s"  ${head.typeName} ${head.fieldName};") { (fields: String, newField: StructField) =>
            fields + "\n" + s"  ${newField.typeName} ${newField.fieldName};"
          }
        })
      }" +
      s"\n} $typeName;"
  }

  def defineStructs(understood: Understood): String = {
    understood.structDefinitions.mkStringMap("\n")((generateDefinition _).tupled)
  }

  def generateHeader(understood: Understood): String = {
    runAndAppend(understood, "\n\n")(declareStructs, declareAliases, defineStructs)
  }

  def generateStaticDeclarations(understood: Understood): String = {
    // TODO: Multiple window support? Probably not
    val windowDeclaration = "static Window *window;"
    val layerDeclarations = understood.layers.mkStringMap("\n") { case (layerName, _) =>
      s"static Layer *$layerName;"
    }
    val signalDeclarations = understood.signals.map { signal: Lexer.Signal =>
      s"""static ${signal.typeName} ${signal.signalName};"""
    } mkString "\n"
    s"$windowDeclaration\n$layerDeclarations\n$signalDeclarations"
  }

  def generateSignalHandlerSignature(signalName: String, signalType: String): String =
    s"void watch_model_${signalName}_change_handler($signalType new_value)"

  def generateSignalHandlers(understood: Understood): String = {
    val functionSigs = understood.signals.map {
      case Lexer.Signal(signalName, signalType) =>
        generateSignalHandlerSignature(signalName, signalType) + " {"
    }
    // TODO: Automatically detect which layers a signal touches?
    val markDirties = understood.layers.mkStringMap("\n") { case (layerName, _) => s"  layer_mark_dirty($layerName);" }
    val setters = understood.signals.map { signal => s"  ${signal.signalName} = new_value;" }
    // TODO: WTF?
    (functionSigs, setters).zipped.map {
      case (sig, setter) => sig + "\n" + setter + "\n" + markDirties + "\n}"
    } mkString "\n\n"
  }

  def generateDrawProcs(understood: Understood): String = {
    understood.layers.mkStringMap("\n") {
      case (layerName, drawProc) =>
        val sig = s"static void ${drawProcName(layerName)}(Layer *layer, GContext *${drawProc.contextName}) {"
        val code = drawProc.code
        sig + "\n" + code + "\n}"
    }
  }

  def drawProcName(layerName: String) = s"${layerName}_draw_proc"

  def generateWindowHandlers(understood: Understood): String = {
    val defineLoad = "static void window_load(Window *window) {"
    val bodyLoad = """  Layer *const window_layer = window_get_root_layer(window);
                     |  const GRect bounds = layer_get_bounds(window_layer);
                     |  window_set_background_color(window, GColorBlack);""".stripMargin
    val layerLoad = understood.layers.mkStringMap("\n") {
      case (layerName, drawProc) =>
        s"  $layerName = layer_create(bounds);\n" +
          s"  layer_set_update_proc($layerName, ${drawProcName(layerName)});\n" +
          s"  layer_add_child(window_layer, $layerName);"
    }
    val defineUnload = "static void window_unload(Window *window) {"
    val layerUnload = understood.layers.mkStringMap("\n") {
      case (layerName, drawProc) => s"  layer_destroy($layerName);"
    }
    val windowLoadCode = s"$defineLoad\n$bodyLoad\n$layerLoad\n}"
    val windowUnloadCode = s"$defineUnload\n$layerUnload"
    val allWindowHandlers = s"$windowLoadCode\n\n$windowUnloadCode\n}"
    allWindowHandlers
  }

  def generateInitAndDeinit(understood: Understood): String = {
    """static void init(void) {
      |  window = window_create();
      |  window_set_window_handlers(window, (WindowHandlers) {
      |    .load = window_load,
      |    .unload = window_unload
      |  });
      |  window_stack_push(window, true /* animated */);
      |}
      |
      |static void deinit(void) {
      |  window_destroy(window);
      |}""".stripMargin
  }

  def generateMainFunction(understood: Understood): String = {
    """int main(void) {
      |  init();
      |  app_event_loop();
      |  deinit();
      |}""".stripMargin
  }

  def generateMainIncludes(understood: Understood): String = """#include "watch_model.h""""

  def generateMainFile(understood: Understood): String = {
    runAndAppend(understood, "\n\n")(generateMainIncludes, generateStaticDeclarations, generateSignalHandlers,
      generateDrawProcs, generateWindowHandlers, generateInitAndDeinit, generateMainFunction)
  }

  def mkProgram(understood: Understood): Map[String, String] = {
    Map("watch_model.h" -> generateHeader(understood),
      "watch_model.c" -> generateModel(understood),
      "watch_main.c" -> generateMainFile(understood))
  }

  def interpolatorName(typeName: String) = {
    if ( """u?int\d\d?_t""".r.findAllIn(typeName).hasNext) "prv_interpolate_int64_t_linear"
    else s"prv_interpolate_${typeName}_linear"
  }

  def generateInterpolators(understood: Understood): String = {
    //TODO: INCLUDE UINT64_T
    val int64Interpolator = """static int64_t prv_interpolate_int64_t_linear(int64_t from, int64_t to, const AnimationProgress progress) {
                              |  return from + ((progress * (to - from)) / ANIMATION_NORMALIZED_MAX);
                              |}""".stripMargin
    val declarations = understood.structDefinitions.mkStringMap("\n") {
      case (structName, fields: Seq[StructField]) =>
        s"static $structName prv_interpolate_${structName}_linear($structName from, $structName to, const AnimationProgress progress);"
    }
    declarations + "\n\n" + int64Interpolator + "\n\n" + understood.structDefinitions.mkStringMap("\n\n") {
      case (structName, fields: Seq[StructField]) =>
        s"static $structName prv_interpolate_${structName}_linear($structName from, $structName to, const AnimationProgress progress) {\n" +
          s"  return ($structName) {\n" + fields.mkStringMap("\n") {
          case StructField(fieldName, typeName) =>
            s"    .$fieldName = ($typeName) ${interpolatorName(typeName)}(from.$fieldName, to.$fieldName, progress),"
        }
    } + "\n  };\n}"
  }

  def generateModelIncludes(understood: Understood): String = """#include "watch_model.h""""

  val translateAnimationCurve = scalaz.Memo.mutableHashMapMemo[AnimationCurve, String] {
    case LinearCurve => "AnimationCurveLinear"
    case EaseOutCurve => "AnimationCurveEaseOut"
    case EaseInCurve => "AnimationCurveEaseIn"
    case EaseInOutCurve => "AnimationCurveEaseInOut"
  }

  def generateAnimations(understood: Understood): String = {
    val updateAnimations = understood.animations.zipWithIndex.mkStringMap("\n\n") {
      case (Animation(durationMs, delayMs, curve, components), index) =>
        s"""static void update_animation_$index(Animation *animation, const AnimationProgress animation_progress) {
          |${components.mkStringMap("\n") {
            case AnimationComponent(signalName, startValue, endValue) =>
              val signal = understood.signals.find(_.signalName == signalName)
              signal.fold {
                throw new RuntimeException("Signal name not found!")
              } { signal =>
                s"  $signalName = ${interpolatorName(signal.typeName)}"
              }
          }
        }
            |}""".stripMargin
    }
    val makeAnimations = understood.animations.zipWithIndex.mkStringMap("\n\n") {
      case (Animation(durationMs, delayMs, curve, Seq(AnimationComponent(signalName, startValue, endValue))), index) =>
        s"static void make_animation_$index(void) {\n" +
          s"  Animation *animation_$index = animation_create();\n" +
          s"  static const AnimationImplementation animation_implementation = {\n" +
          s"    .update = update_animation_$index,\n" +
          s"  };\n" +
          s"  animation_set_implementation(animation_$index, &animation_implementation);\n" +
          s"  animation_set_curve(animation_$index, ${translateAnimationCurve(curve)});\n" +
          s"  animation_set_delay(animation_$index, $delayMs);\n" +
          s"  animation_set_duration(animation_$index, $durationMs);\n" +
          s"}"
    }
    updateAnimations + "\n\n" + makeAnimations
  }

  def generateStartIntroAnimation(understood: Understood): String = {
    s"""void watch_model_start_intro_animation(void) {
       | Animation *animation;
       |${
      understood.animations.zipWithIndex.mkStringMap("\n") {
        case (animation, index) =>
          s"  animation = make_animation_"
      }
    }
     """.stripMargin
  }

  def generateModelInit(understood: Understood): String = ???

  def generateModel(understood: Understood): String = {
    runAndAppend(understood, "\n\n")(generateInterpolators, generateModelIncludes, generateAnimations,
      generateStartIntroAnimation, generateModelInit)
  }
}