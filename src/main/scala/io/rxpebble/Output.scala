package io.rxpebble

import io.rxpebble.Lexer.{StructField, Understood}

object Output {
  def generateDeclaration(typeName: Lexer.Type, structFields: Seq[StructField]): String = {
    s"  struct $typeName;\n  typedef struct $typeName $typeName;"
  }

  def declareStructs(understood: Understood): String = {
    understood.structDefinitions.map((generateDeclaration _).tupled).mkString("\n")
  }

  def generateAlias(typeName: Lexer.Type, aliasTo: Lexer.Type): String = {
    s"  typedef $typeName $aliasTo;"
  }

  def declareAliases(understood: Understood): String = {
    understood.typeAliases.map((generateAlias _).tupled).mkString("\n")
  }

  def generateDefinition(typeName: Lexer.Type, structFields: Seq[StructField]): String = {

  }

  def defineStructs(understood: Understood): String = {
    understood.structDefinitions.map((generateDefinition _).tupled).mkString("\n")
  }

  def generateHeader(understood: Understood): String = {
    Vector(declareStructs(understood), declareAliases(understood), defineStructs(understood)).mkString("\n\n")
  }

  def mkProgram(understood: Understood): Map[String, String] = {
    Map("watch_model.h" -> generateHeader(understood))
  }
}