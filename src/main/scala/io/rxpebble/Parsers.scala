package io.rxpebble

import fastparse.all._
import fastparse.core.Result.Failure
import io.rxpebble.Lexer.{EaseOutCurve, EaseInOutCurve, EaseInCurve, LinearCurve}
import shapeless.ops.hlist.Tupler

object Parsers {

  sealed trait Statement
  case class SignalDeclaration(signalIdentifier: String, typeIdentifier: String) extends Statement
  case class TypeDeclaration(typeIdentifier: String, typeDefinition: Either[TypeStructDeclaration, TypeAliasDeclaration]) extends Statement
  case class LayerDeclaration(layerName: String, gContextName: String, drawProcCode: String) extends Statement
  case class AnimationComponent(signalName: String, fromValue: String, toValue: String)
  case class AnimationDefinition(chained: Boolean, curve: Lexer.AnimationCurve, duration: Int, delay: Int, components: Seq[AnimationComponent])
  case class CompoundAnimationDefinition(animations: Seq[AnimationDefinition])
  case class ForeverDefinition(timeName: String, code: String)
  case class StageDefinition(definitions: Seq[AnimationDefinition], forever: ForeverDefinition) extends Statement
  case class TypeStructDeclaration(fields: Seq[TypeStructField])
  case class TypeStructField(fieldName: String, typeName: String)
  case class TypeAliasDeclaration(aliasTo: String)

  val whiteSpace = P(CharPred(Character.isWhitespace).rep)
  val wsp = whiteSpace
  val newline = wsp.? ~ "\n" ~ wsp.?
  val ident = P(CharPred(Character.isAlphabetic(_)) ~ CharPred(ch => Character.isAlphabetic(ch) || Character.isDigit(ch) || "_-".contains(ch)).rep)
  val typeName = P(ident)
  val signalName = P(ident)
  val fieldName = P(ident)
  val layerName = P(ident)
  val animationName = P(ident)
  val animationCurve: Parser[Lexer.AnimationCurve] = P(
    P("linear").map(_ => LinearCurve) | P("ease_out").map(_ => EaseOutCurve) | P("ease_in").map(_ => EaseInCurve) | P("ease_in_out").map(_ => EaseInOutCurve)
  )
  val timePeriod: Parser[Int] = P(CharIn("-").?.! ~ CharPred(Character.isDigit).! ~ "ms").map {
    case (neg, number) => (neg + number).toInt
  }
  val signalValue = P(CharIn('0' to '9', 'a' to 'z', 'A' to 'Z') | CharPred(Character.isWhitespace))
  val signalDeclaration: Parser[SignalDeclaration] = P("signal" ~ wsp ~ signalName.! ~ wsp.? ~ ":" ~ wsp.? ~ typeName.!).map(SignalDeclaration.tupled)
  val typeAliasDeclaration: Parser[TypeAliasDeclaration] = P(typeName.!).map(TypeAliasDeclaration)
  val typeStructFieldDeclaration: Parser[TypeStructField] = P(fieldName.! ~ wsp.? ~ ":" ~ wsp.? ~ typeName.!).map(TypeStructField.tupled)
  val typeStructFields: Parser[Seq[TypeStructField]] = P(typeStructFieldDeclaration.rep(sep = wsp.? ~ ",\n" ~ wsp.?, min = 1))
  val typeStructDeclaration: Parser[TypeStructDeclaration] = "{" ~ wsp.? ~ typeStructFields.map(TypeStructDeclaration) ~ wsp.? ~ "}"
  val structOrAlias: Parser[Either[TypeStructDeclaration, TypeAliasDeclaration]] = P(typeStructDeclaration.map(Left.apply) | typeAliasDeclaration.map(Right.apply))
  val typeDeclaration: Parser[TypeDeclaration] = P("type" ~ wsp ~ typeName.! ~ wsp.? ~ "=" ~ wsp.? ~ structOrAlias).map(TypeDeclaration.tupled)
  val drawProcDefinition: Parser[(String, String)] = P("(" ~ wsp.? ~ ident.! ~ ")" ~ wsp.? ~ "{" ~ CharPred(CharPredicates.isPrintableChar).! ~ "} end")
  val layerDeclaration: Parser[Statement] = P("layer" ~ wsp ~ layerName.! ~ wsp.? ~ "=" ~ wsp.? ~ drawProcDefinition)
    .map(t => LayerDeclaration.tupled((t._1, t._2._1, t._2._2)))
  val constantDeclaration: Parser[Statement] = P(Fail)
  val optionalDelay: Parser[Option[Int]] = P("after" ~ wsp ~ timePeriod ~ wsp).map(Some(_)) | P(Pass).map(_ => None)
  val animationComponent: Parser[AnimationComponent] = P(signalName.! ~ wsp ~ "from" ~ wsp ~ signalValue.! ~ wsp ~ "to" ~ wsp ~ signalValue.!).map(AnimationComponent.tupled)
  val animationDefinition: Parser[AnimationDefinition] = P(
    (("then" ~ wsp).?.map(_ ⇒ true) | Pass.map(_ ⇒ false)) ~ animationCurve ~ wsp ~ "for" ~ wsp ~ timePeriod ~ wsp ~ optionalDelay ~ "{\n" ~ animationComponent.rep(min = 1, sep = "\n") ~ "\n}").map {
    case (chained, curve, duration, maybeDelay, components) => AnimationDefinition(chained, curve, duration, maybeDelay.getOrElse(0), components)
  }

  val stageDeclaration: P[Statement] = P("stage" ~ wsp.? ~ "{" ~ stageDefinition ~ wsp.? ~ "}")
  val stageDefinition: P[StageDefinition] =
  val statement: Parser[Statement] = P(signalDeclaration | typeDeclaration | layerDeclaration | constantDeclaration | stageDeclaration)
  val program: Parser[Seq[Statement]] = P(statement.rep(min = 1, sep = ("\n" | wsp.? ~ "\n") ~ wsp.?) ~ wsp.? ~ End)
}
