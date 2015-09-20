package io.rxpebble

import io.rxpebble.Lexer.{StructField, Understood}

object Output {
  def generateDeclaration(typeName: Lexer.Type, structFields: Seq[StructField]): String = {
    s"struct $typeName;\ntypedef struct $typeName $typeName;"
  }

  def declareStructs(understood: Understood): String = {
    understood.structDefinitions.map((generateDeclaration _).tupled).mkString("\n")
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
          tail.foldLeft[String](s"  ${head.typeName} ${head.fieldName};"){ (fields: String, newField: StructField) =>
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
    Vector(declareStructs(understood), declareAliases(understood), defineStructs(understood)).mkString("\n\n")
  }

  def generateMain(understood: Understood): String = ???

  def generateModel(understood: Understood): String = ???

  def mkProgram(understood: Understood): Map[String, String] = {
    Map("watch_model.h" -> generateHeader(understood),
    "watch_model.c" -> generateModel(understood),
    "watch_main.c" -> generateMain(understood))
  }
}