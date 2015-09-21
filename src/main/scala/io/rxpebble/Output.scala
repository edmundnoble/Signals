package io.rxpebble

import io.rxpebble.Lexer.{StructField, Understood}


object Output {
  def runAndAppend[T](dep: T, delim: String)(funs: (T => String)*): String = funs.map(_(dep)).mkString(delim)
  def generateForwardTypeDeclaration(typeName: Lexer.Type, structFields: Seq[StructField]): String = {
    s"struct $typeName;\ntypedef struct $typeName $typeName;"
  }

  def declareStructs(understood: Understood): String = {
    understood.structDefinitions.map((generateForwardTypeDeclaration _).tupled).mkString("\n")
  }

  def generateAlias(typeName: Lexer.Type, aliasTo: Lexer.Type): String = {
    s"typedef $typeName $aliasTo;"
  }

  def declareAliases(understood: Understood): String = {
    understood.typeAliases.map((generateAlias _).tupled).mkString("\n")
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
    understood.structDefinitions.map((generateDefinition _).tupled).mkString("\n")
  }

  def generateHeader(understood: Understood): String = {
    runAndAppend(understood, "\n\n")(declareStructs, declareAliases, defineStructs)
  }

  def generateStaticDeclarations(understood: Understood): String = {
    // TODO: Multiple window support? Probably not
    val windowDeclaration = "static Window *window;"
    val layerDeclarations = understood.layers.map { case (layerName: String, drawProc: Lexer.DrawProc) =>
      ???
    }.mkString("\n")
    val signalDeclarations = understood.signals.map { signal: Lexer.Signal =>
      s"""static ${signal.typeName} ${signal.signalName};"""
    }.mkString("\n")
    Vector(windowDeclaration, layerDeclarations, signalDeclarations).mkString("\n")
  }

  def generateSignalHandlers(understood: Understood): String = {
    ???
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