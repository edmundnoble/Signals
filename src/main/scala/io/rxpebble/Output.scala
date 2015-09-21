package io.rxpebble

import io.rxpebble.Lexer.{StructField, Understood}

object Output {
  implicit class WithMkStringMap[A](val ts: TraversableOnce[A]) extends AnyVal {
    def mkStringMap[B](delim: String)(f: A => B): String = {
      if (ts.isEmpty) {
        ""
      } else {
        val builder = new StringBuilder()
        var first = true
        for (a <- ts) {
          builder.append(f(a))
          if (!first) {
            builder.append(delim)
          }
          first = false
        }
        builder.toString()
      }
    }
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
    val signalDeclarations = understood.signals.mkStringMap("\n") { signal: Lexer.Signal =>
      s"""static ${signal.typeName} ${signal.signalName};"""
    }
    Vector(windowDeclaration, layerDeclarations, signalDeclarations).mkString("\n")
  }

  def generateSignalHandlers(understood: Understood): String = {
    val functionSigs = understood.signals.map { case Lexer.Signal(signalName, signalType) => s"static void watch_model_${signalName}_change_handler($signalType new_value) {"}
    // TODO: Automatically detect which layers a signal touches?
    val markDirties = understood.layers.mkStringMap("\n") { case (layerName, _) => s"  layer_mark_dirty($layerName);" }
    val setters = understood.signals.map { signal => s"  ${signal.signalName} = new_value;"}
    // TODO: WTF?
    runtime.ZippedTraversable2.zippedTraversable2ToTraversable((functionSigs, setters).zipped).mkStringMap("\n\n") {
      case (sig, setter) => sig + "\n" + markDirties + "\n" + setter + "\n}"
    }
  }

  def generateDrawProc(understood: Understood): String = {
    ???
  }

  def generateWindowHandlers(understood: Understood): String = {
    val defineLoad = "static void window_load(Window *window) {"
    val body = """"""
    ???
  }

  def generateInitAndDeinit(understood: Understood): String = {
    ???
  }

  def generateMainFunction(understood: Understood): String = {
    ???
  }

  def generateMainFile(understood: Understood): String = {
    runAndAppend(understood, "\n")(generateStaticDeclarations, generateSignalHandlers, generateDrawProc, generateWindowHandlers,
      generateInitAndDeinit, generateMainFunction)
  }

  def generateModel(understood: Understood): String = ???

  def mkProgram(understood: Understood): Map[String, String] = {
    Map("watch_model.h" -> generateHeader(understood),
      "watch_model.c" -> generateModel(understood),
      "watch_main.c" -> generateMainFile(understood))
  }
}