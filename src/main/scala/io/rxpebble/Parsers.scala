package io.rxpebble

import fastparse.all._
import fastparse.core.Result.Failure

object Parsers {
  sealed trait Statement
  case class SignalDeclaration(signalIdentifier: String, typeIdentifier: String) extends Statement
  case class TypeDeclaration(typeIdentifier: String, typeDefinition: Either[TypeStructDeclaration, TypeAliasDeclaration]) extends Statement
  case class TypeStructDeclaration(fields: Seq[TypeStructField])
  case class TypeStructField(fieldName: String, typeName: String)
  case class TypeAliasDeclaration(aliasTo: String)
  val whiteSpace = P(CharPred(Character.isWhitespace).rep)
  val wsp = whiteSpace
  val ident = P(CharPred(Character.isAlphabetic(_)) ~ CharPred(ch => Character.isAlphabetic(ch) || Character.isDigit(ch) || "_-".contains(ch)).rep)
  val typeName = P(ident.!)
  val signalName = P(ident.!)
  val fieldName = P(ident.!)
  val signalDeclaration: Parser[SignalDeclaration] = P("signal" ~ wsp ~ signalName.! ~ wsp.? ~ ":" ~ wsp.? ~ typeName.!).map(SignalDeclaration.tupled)
  val typeAliasDeclaration: Parser[TypeAliasDeclaration] = P(typeName.!).map(TypeAliasDeclaration)
  val typeStructFieldDeclaration: Parser[TypeStructField] = P(fieldName.! ~ wsp.? ~ ":" ~ wsp.? ~ typeName.!).map(TypeStructField.tupled)
  val typeStructFields: Parser[Seq[TypeStructField]] = P(typeStructFieldDeclaration.rep(sep = wsp.? ~ ",\n" ~ wsp.?, min = 1))
  val typeStructDeclaration: Parser[TypeStructDeclaration] = "{" ~ wsp.? ~ typeStructFields.map(TypeStructDeclaration) ~ wsp.? ~ "}"
  val structOrAlias: Parser[Either[TypeStructDeclaration, TypeAliasDeclaration]] = P(typeStructDeclaration.map(Left.apply) | typeAliasDeclaration.map(Right.apply))
  val typeDeclaration: Parser[TypeDeclaration] = P("type" ~ wsp ~ typeName.! ~ wsp.? ~ "=" ~ wsp.? ~ structOrAlias).map(TypeDeclaration.tupled)
  val drawProcDeclaration: Parser[Statement] = P(Fail)
  val constantDeclaration: Parser[Statement] = P(Fail)
  val statement: Parser[Statement] = P(signalDeclaration | typeDeclaration | drawProcDeclaration | constantDeclaration)
  val program: Parser[Seq[Statement]] = P(statement.rep(min = 1, sep = ("\n" | wsp.? ~ "\n") ~ wsp.?) ~ wsp.? ~ End)
}
