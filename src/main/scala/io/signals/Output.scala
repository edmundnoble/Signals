package io.signals

import io.signals.Compiler._
import io.signals.Parsers.{ConstantAnimationComponent, AnimationComponent, InterpolatedAnimationComponent}

import scalaz._
import Scalaz._

object Output {
  implicit class DelimAppend(str: String) {
    def +|+(other: String) = if (str.forall(Character.isWhitespace)) other else str + other
  }
  implicit class WithMkStringMap[A](val ts: TraversableOnce[A]) extends AnyVal {
    def mkStringMap[B](delim: String)(f: A => B): String = {
      // ts.map(f).mkString(delim)
      val sb = new StringBuilder()
      var first = true
      for (a ← ts) {
        val comp = f(a).toString
        if (!comp.isEmpty) {
          if (first) {
            sb.append(comp)
            first = false
          } else {
            sb.append(delim)
            sb.append(comp)
          }
        }
      }
      sb.toString()
    }
  }

  def runAndAppend[T](param: T, delim: String)(funs: (T => String)*): String = funs.mkStringMap(delim)(_(param))

  def generateForwardTypeDeclaration(typeName: Compiler.Type, structDef: StructDef): String = {
    if (structDef.native) {
      ""
    } else {
      s"struct $typeName;\ntypedef struct $typeName $typeName;"
    }
  }

  def declareStructs(understood: Understood): String = {
    understood.structDefinitions.mkStringMap("\n")((generateForwardTypeDeclaration _).tupled)
  }

  def generateAlias(typeName: Compiler.Type, aliasTo: Compiler.Type): String = {
    s"typedef $typeName $aliasTo;"
  }

  def declareAliases(understood: Understood): String = {
    understood.typeAliases.mkStringMap("\n")((generateAlias _).tupled)
  }

  def generateDefinition(typeName: Compiler.Type, structDef: StructDef): String = {
    if (!structDef.native) {
      s"typedef struct $typeName {\n" +
        s"${
          structDef.fields.headOption.fold("")({ head =>
            val tail = structDef.fields.tail
            tail.foldLeft[String](s"  ${head.typeName} ${head.fieldName};") { (fields: String, newField: StructField) =>
              fields + "\n" + s"  ${newField.typeName} ${newField.fieldName};"
            }
          })
        }" +
        s"\n} $typeName;"
    } else {
      ""
    }
  }

  def defineStructs(understood: Understood): String = {
    understood.structDefinitions.mkStringMap("\n")((generateDefinition _).tupled)
  }

  def generateHeaderIncludes(understood: Understood): String = {
    "#include <pebble.h>"
  }

  def declareSignalHandlers(understood: Understood): String = {
    understood.signals.mkStringMap("\n")(generateSignalHandlerDeclaration)
  }

  def declareModelInit(understood: Understood): String = {
    "void watch_model_init(void);"
  }

  def generateHeader(understood: Understood): String = {
    runAndAppend(understood, "\n\n")(generateHeaderIncludes, declareStructs, declareAliases, defineStructs,
      declareSignalHandlers, declareModelInit)
  }

  def generateStaticDeclarations(understood: Understood): String = {
    // TODO: Multiple window support? Probably not
    val windowDeclaration = "static Window *window;"
    val layerDeclarations = understood.layers.mkStringMap("\n") { case (layerName, _) =>
      s"static Layer *$layerName;"
    }
    val signalDeclarations = understood.signals.map { signal: Compiler.Signal =>
      s"""static ${signal.typeName} ${signal.signalName};"""
    } mkString "\n"
    s"$windowDeclaration\n$layerDeclarations\n$signalDeclarations"
  }

  def generateSignalHandlerName(signalName: String): String = s"watch_model_${signalName}_change_handler"

  def generateSignalHandlerSignature(signalName: String, signalType: String): String =
    s"void ${generateSignalHandlerName(signalName)}($signalType new_value)"

  def generateSignalHandlerDeclaration(signal: Signal): String =
    s"${generateSignalHandlerSignature(signal.signalName, signal.typeName)};"

  def generateSignalHandlers(understood: Understood): String = {
    val functionSigs = understood.signals.map {
      case Compiler.Signal(signalName, signalType) =>
        generateSignalHandlerSignature(signalName, signalType) + " {"
    }
    // TODO: Automatically detect which layers a signal touches?
    val markDirties = understood.layers.mkStringMap("\n") { case (layerName, _) => s"  layer_mark_dirty($layerName);" }
    val setters = understood.signals.map { signal => s"  ${signal.signalName} = new_value;" }
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
      case (layerName, _) =>
        s"  $layerName = layer_create(bounds);\n" +
          s"  layer_set_update_proc($layerName, ${drawProcName(layerName)});\n"
    }
    val layerAddChild = understood.layers.mkStringMap("\n") {
      case (layerName, _) ⇒
        s"  layer_add_child(window_layer, $layerName);"
    }
    val modelLoad = s"  watch_model_init();"
    val defineUnload = "static void window_unload(Window *window) {"
    val layerUnload = understood.layers.mkStringMap("\n") {
      case (layerName, drawProc) => s"  layer_destroy($layerName);"
    }
    val windowLoadCode = s"$defineLoad\n$bodyLoad\n$layerLoad\n$modelLoad\n$layerAddChild}"
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
      case (structName, _) =>
        s"static $structName ${interpolatorName(structName)}($structName from, $structName to, const AnimationProgress progress);"
    }

    declarations + "\n\n" + int64Interpolator + "\n\n" + understood.structDefinitions.mkStringMap("\n\n") {
      case (structName, structDef) =>
        s"static $structName ${interpolatorName(structName)}($structName from, $structName to, const AnimationProgress progress) {\n" +
          s"  return ($structName) {\n" +
          structDef.fields.mkStringMap("\n") {
            case StructField(fieldName, typeName) =>
              s"    .$fieldName = ($typeName) ${interpolatorName(typeName)}(from.$fieldName, to.$fieldName, progress),"
          } + "\n  };\n}"
    }
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
      case (Animation(durationMs, delayMs, curve, components, temps), index) =>
        s"""static void update_animation_$index(Animation *animation, const AnimationProgress animation_progress) {
                                                 |${
          temps.mkStringMap("\n")(_.body) + "\n" + components.mkStringMap("\n") {
            case InterpolatedAnimationComponent(signalName, startValue, endValue) =>
              val signal = understood.signals.find(_.signalName == signalName)
              signal.fold {
                throw new RuntimeException("Signal name not found!")
              } { signal =>
                s"  ${generateSignalHandlerName(signalName)}(${interpolatorName(signal.typeName)}($startValue, $endValue, animation_progress));"
              }
            case ConstantAnimationComponent(signalName, constantValue) ⇒
              s"  ${generateSignalHandlerName(signalName)}($constantValue);"
          }
        }
            |}""".stripMargin
    }

    val relatedAnimations = understood.signals.map(sig ⇒ sig → understood.animations.filter(_.components.exists {
      case InterpolatedAnimationComponent(signalName, _, _) ⇒ signalName == sig.signalName
      case ConstantAnimationComponent(signalName, _) ⇒ signalName == sig.signalName
    }))
    val maxAnimations = relatedAnimations.flatMap {
      case (sig, anims) ⇒ if (anims.isEmpty) None else Some((sig, anims.minBy(anim ⇒ anim.durationMs + anim.delayMs)))
    }
    val minAnimations = relatedAnimations.flatMap {
      case (sig, anims) ⇒ if (anims.isEmpty) None else Some((sig, anims.minBy(_.delayMs)))
    }
    val makeAnimations = understood.animations.zipWithIndex.mkStringMap("\n\n") {
      case (anim@Animation(durationMs, delayMs, curve, _, _), index) =>
        val baseMakeAnimation = s"static Animation *make_animation_$index(void) {\n" +
          s"  Animation *animation_$index = animation_create();\n" +
          s"  static const AnimationImplementation animation_implementation = {\n" +
          s"    .update = update_animation_$index,\n" +
          s"  };\n"
        val finishMakeAnimation =
          s"  animation_set_implementation(animation_$index, &animation_implementation);\n" +
            s"  animation_set_curve(animation_$index, ${translateAnimationCurve(curve)});\n" +
            s"  animation_set_delay(animation_$index, $delayMs);\n" +
            s"  animation_set_duration(animation_$index, $durationMs);\n" +
            s"  return animation_$index;\n" +
            s"}"
        val lastAnimationFor = maxAnimations.filter { case (sig, otherAnim) ⇒ otherAnim == anim }
        val firstAnimationFor = minAnimations.filter { case (sig, otherAnim) ⇒ otherAnim == anim }
        val stoppedAnimationHandler = if (lastAnimationFor.nonEmpty) {
          s"static void stopped_animation_${index}_handler(Animation *animation, bool finished, void *context) {\n" +
            lastAnimationFor.mkStringMap("\n") {
              case (sig, otherAnim) ⇒ s"  ${isAnimatedName(sig.signalName)} = false;"
            } +
            s"\n}"
        } else {
          ""
        }
        val startedAnimationHandler = if (firstAnimationFor.nonEmpty) {
          s"static void started_animation_${index}_handler(Animation *animation, void *context) {\n" +
            firstAnimationFor.mkStringMap("\n") {
              case (sig, otherAnim) ⇒ s"  ${isAnimatedName(sig.signalName)} = true;"
            } +
            s"\n}"
        } else {
          ""
        }
        val setHandlers = if (firstAnimationFor.nonEmpty && lastAnimationFor.nonEmpty) {
          s"  animation_set_handlers(animation_$index,\n" +
            s"                       (AnimationHandlers){ .stopped = stopped_animation_${index}_handler,\n" +
            s"                                            .started = started_animation_${index}_handler },\n" +
            s"                       NULL);"
        } else if (firstAnimationFor.nonEmpty) {
          s"  animation_set_handlers(animation_$index,\n" +
            s"                       (AnimationHandlers) { .started = started_animation_${index}_handler },\n" +
            s"                       NULL) ;" +
            finishMakeAnimation
        } else if (lastAnimationFor.nonEmpty) {
          s"  animation_set_handlers(animation_$index,\n" +
            s"                       (AnimationHandlers) { .stopped = stopped_animation_${index}_handler },\n" +
            s"                       NULL );"
        } else {
          ""
        }
        stoppedAnimationHandler +|+ "\n\n" +|+ startedAnimationHandler +|+ "\n\n" +|+ baseMakeAnimation +|+ setHandlers +|+
          "\n" +|+ finishMakeAnimation
    }
    updateAnimations +|+ "\n\n" +|+ makeAnimations
  }

  def generateStartIntroAnimation(understood: Understood): String = {
    val signature = "void watch_model_start_intro_animation(void) {"
    val startAnimations =
      if (understood.animations.isEmpty) {
        ""
      } else {
        val allAnimations = if (understood.animations.tail.isEmpty) {
          s"  Animation *all_animations = make_animation_0();\n"
        } else {
          understood.animations.indices.mkStringMap("\n") { index ⇒
            s"  Animation *animation_$index = make_animation_$index();"
          } +
            s"\n  Animation *all_animations = animation_spawn_create(${understood.animations.indices.map(i ⇒ s"animation_$i").mkString(", ")});\n"
        }
        allAnimations + "  animation_schedule(all_animations);"
      }
    signature + "\n" + startAnimations + "\n}"
  }

  def componentHasSignal(signal: String)(comp: AnimationComponent): Boolean = {
    comp match {
      case InterpolatedAnimationComponent(compSignal, _, _) ⇒ compSignal == signal
      case ConstantAnimationComponent(compSignal, _) ⇒ compSignal == signal
    }
  }

  def generateModelInit(understood: Understood): String = {
    val didFocus = "static void prv_app_focus_handler(bool in_focus) {\n" +
      "  if (!in_focus) { return; }\n" +
      "  watch_model_start_intro_animation();\n" +
      "}"
    val signature = s"void watch_model_init(void) {"
    val signalInit = understood.signals.map { signal ⇒
      val components =
        understood.animations.map(a ⇒ (a.delayMs, a.components.filter(componentHasSignal(signal.signalName)))).filter(_._2.nonEmpty)
      if (components.isEmpty) {
        println(s"All components: ${understood.animations.flatMap(_.components)}")
        println(s"Couldn't find animation for start of ${signal.signalName}")
        s"  ${isAnimatedName(signal.signalName)} = false;"
      } else {
        println(s"Found animation for start of ${signal.signalName}")
        val firstAnimationComponent = components.minBy(_._1)._2.head
        s"  ${isAnimatedName(signal.signalName)} = true;\n" + (firstAnimationComponent match {
          case InterpolatedAnimationComponent(_, fromValue, _) ⇒
            s"  ${generateSignalHandlerName(signal.signalName)}($fromValue);"
          case ConstantAnimationComponent(_, constantValue) ⇒
            s"  ${generateSignalHandlerName(signal.signalName)}($constantValue);"
        })
      }
    }.filter(!_.isEmpty).mkString("\n")
    val enableTickTimer = "  tick_timer_service_subscribe(MINUTE_UNIT, prv_tick_time_handler);"
    val enableAppFocusHandler = "  app_focus_service_subscribe_handlers((AppFocusHandlers) { .did_focus = prv_app_focus_handler });"
    didFocus + "\n\n" + signature + "\n" + signalInit + "\n" + enableTickTimer + "\n" + enableAppFocusHandler + "\n}"
  }

  def isAnimatedName(signalName: String) = s"s_is_animated_$signalName"

  def generateForevers(understood: Understood): String = {
    val foreverSignature = "static void prv_tick_time_handler(struct tm *tick_time, TimeUnits units_changed) {"
    val temps = understood.forever.temps.mkStringMap("\n")(_.body)
    val timeHandlerRunners = understood.signals.mkStringMap("\n") { s ⇒
      s"  if (!${isAnimatedName(s.signalName)}) {\n" +
        s"    ${generateSignalHandlerName(s.signalName)}(${understood.forever.components.find(_.signalName == s.signalName).get.value});\n" +
        s"  }"
    }
    foreverSignature + "\n" + temps + "\n" + timeHandlerRunners + "\n}"
  }

  def generateAnimationState(understood: Understood): String = {
    understood.signals.mkStringMap("\n") { signal ⇒
      s"static bool ${isAnimatedName(signal.signalName)};"
    }
  }

  def generateFunctions(understood: Understood): String = {
    understood.functions.mkStringMap("\n")(f => s"${f.functionSignature} {\n${f.functionBody}\n}")
  }

  def generateModel(understood: Understood): String = {
    runAndAppend(understood, "\n\n")(generateModelIncludes, generateInterpolators, generateAnimationState,
      generateFunctions, generateAnimations, generateForevers, generateStartIntroAnimation, generateModelInit)
  }
}