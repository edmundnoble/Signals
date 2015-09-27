package io.rxpebble

import fastparse.Logger
import io.rxpebble.Lexer.{EaseOutCurve, EaseInOutCurve, EaseInCurve, LinearCurve}
import shapeless.ops.hlist.Tupler

import scala.language.postfixOps

object Parsers {

  import fastparse.all._

  sealed trait Statement
  case class SignalDeclaration(signalIdentifier: String, typeIdentifier: String) extends Statement
  case class TypeDeclaration(typeIdentifier: String, typeDefinition: Either[TypeStructDeclaration, TypeAliasDeclaration]) extends Statement
  case class LayerDeclaration(layerName: String, gContextName: String, drawProcCode: String) extends Statement
  case class AnimationComponent(signalName: String, fromValue: String, toValue: String)
  case class AnimationDefinition(chained: Boolean, curve: Lexer.AnimationCurve, duration: Int, delay: Int, components: Seq[AnimationComponent])
  case class IntroDefinition(animations: Seq[AnimationDefinition])
  case class ForeverDefinition(timeName: String, code: String)
  case class StageDefinition(definitions: Seq[AnimationDefinition], forever: ForeverDefinition) extends Statement
  case class TypeStructDeclaration(fields: Seq[TypeStructField])
  case class TypeStructField(fieldName: String, typeName: String)
  case class TypeAliasDeclaration(aliasTo: String)

  def unordered[A, B](pa: Parser[A], pb: Parser[B], sep: Parser[_]): Parser[(A, B)] = {
    val sepNoCapture = sep.map(_ ⇒ ())
    (pa ~ sepNoCapture ~ pb) | (pb ~ sepNoCapture ~ pa).map(_.swap)
  }

  val whiteSpace = P(CharPred(Character.isWhitespace).rep)
  val wsp = whiteSpace
  val newline = "\n"
  val ident = P(CharIn('0' to '9', 'a' to 'z', 'A' to 'Z', "-_").rep(min = 1))
  val typeName = P(ident)
  val signalName = P(ident)
  val fieldName = P(ident)
  val layerName = P(ident)
  val animationName = P(ident)
  val tickTimeName = P(ident)
  val animationCurve: Parser[Lexer.AnimationCurve] = P(
    P("linear").map(_ => LinearCurve) | P("ease_out").map(_ => EaseOutCurve) | P("ease_in").map(_ => EaseInCurve) | P("ease_in_out").map(_ => EaseInOutCurve)
  )
  val timePeriod: Parser[Int] = P(CharIn("-").?.! ~ CharsWhile(Character.isDigit).! ~ "ms").map {
    case (neg, number) => (neg + number).toInt
  }
  val signalValue = P(CharsWhile(ch ⇒ !"\n}".contains(ch)))
  val signalDeclaration: Parser[SignalDeclaration] = P("signal" ~ wsp ~ typeName.! ~ wsp ~ signalName.! ~ wsp.? ~ ";" ~ newline).map {
    case (typeIdentifier, signalIdentifier) ⇒ SignalDeclaration(signalIdentifier = signalIdentifier, typeIdentifier = typeIdentifier)
  }
  val typeAliasDeclaration: Parser[TypeAliasDeclaration] = P(typeName.!).map(TypeAliasDeclaration)
  val typeStructFieldDeclaration: Parser[TypeStructField] = P(fieldName.! ~ ":" ~ typeName.!).map(TypeStructField.tupled)
  val typeStructFields: Parser[Seq[TypeStructField]] = P(typeStructFieldDeclaration.rep(sep = ",\n", min = 1))
  val typeStructDeclaration: Parser[TypeStructDeclaration] = "{" ~ typeStructFields.map(TypeStructDeclaration) ~ "}"
  val structOrAlias: Parser[Either[TypeStructDeclaration, TypeAliasDeclaration]] = P(typeStructDeclaration.map(Left.apply) | typeAliasDeclaration.map(Right.apply))
  val typeDeclaration: Parser[TypeDeclaration] = P("type" ~ wsp ~ typeName.! ~ "=" ~ structOrAlias).map(TypeDeclaration.tupled)
  val drawProcDefinition: Parser[(String, String)] = P("(" ~ ident.! ~ ")" ~ wsp.? ~ "=>" ~ wsp.? ~ "{" ~ CharsWhile(ch ⇒ ch != '}').! ~ "}")
  val layerDeclaration: Parser[Statement] = P("layer" ~ wsp ~ layerName.! ~ wsp.? ~ "=" ~ wsp.? ~ drawProcDefinition)
    .map(t => LayerDeclaration.tupled((t._1, t._2._1, t._2._2)))
  val constantDeclaration: Parser[Statement] = P(Fail)
  val optionalDelay: Parser[Option[Int]] = P("after" ~ wsp ~ timePeriod ~ wsp).map(Some(_)) | P(Pass).map(_ => None)
  val animationComponent: Parser[AnimationComponent] = P(signalName.! ~ wsp ~ "from" ~ wsp ~ signalValue.! ~ wsp ~ "to" ~ wsp ~ signalValue.!).map(AnimationComponent.tupled)
  val animationDefinition: Parser[AnimationDefinition] = P(
    (("then" ~ wsp).map(_ ⇒ true) | Pass.map(_ ⇒ false)) ~ animationCurve ~ wsp ~ "for" ~ wsp ~ timePeriod ~ wsp ~ optionalDelay ~
      wsp.? ~ "{" ~ wsp.? ~ animationComponent.rep(min = 0, sep = wsp.?) ~ wsp.? ~ "}").map {
    case (chained, curve, duration, maybeDelay, components) => AnimationDefinition(chained, curve, duration, maybeDelay.getOrElse(0), components)
  }
  val introDefinition = P("intro" ~ wsp.? ~ "{" ~ wsp.? ~ animationDefinition.rep(min = 1, sep = wsp.?) ~ wsp.? ~ "}")
  val foreverDefinition = P("forever" ~ wsp.? ~ "(" ~ wsp.? ~ tickTimeName.! ~ wsp.? ~ ")" ~ wsp.? ~ "=>" ~ wsp.? ~ "{" ~ CharsWhile(_ != '}').! ~ "}").map(ForeverDefinition.tupled)
  val stageDefinition: P[StageDefinition] = P(unordered(introDefinition, foreverDefinition, wsp.?)).map(StageDefinition.tupled)
  val stageDeclaration: P[Statement] = P("stage" ~ wsp.? ~ "{" ~ wsp.? ~ stageDefinition ~ wsp.? ~ "}")
  val statement: Parser[Statement] = P(signalDeclaration | typeDeclaration | layerDeclaration | constantDeclaration | stageDeclaration)
  val program: Parser[Seq[Statement]] = P(Start ~ wsp.? ~ statement.rep(min = 1, sep = wsp.?) ~ wsp.? ~ End)
}
