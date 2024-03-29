package io.signals

import java.util.concurrent.TimeUnit

import fastparse.Logger
import io.signals.Compiler.{EaseOutCurve, EaseInOutCurve, EaseInCurve, LinearCurve}
import shapeless.ops.hlist.Tupler

import scala.language.postfixOps

object Parsers {

  import fastparse.all._

  implicit class ParserSequenceOps[T](p: Parser[T]) {
    def >>[V](value: V): P[V] = p.map(_ ⇒ value)
  }

  sealed trait Statement
  case class SignalDeclaration(signalIdentifier: String, typeIdentifier: String) extends Statement
  case class TypeDeclaration(typeIdentifier: String, typeDefinition: Either[TypeStructDeclaration, TypeAliasDeclaration]) extends Statement
  case class LayerDeclaration(layerName: String, gContextName: String, drawProcCode: String) extends Statement
  sealed trait AnimationComponent
  case class ConstantAnimationComponent(signalName: String, value: String) extends AnimationComponent
  case class InterpolatedAnimationComponent(signalName: String, fromValue: String, toValue: String) extends AnimationComponent
  case class AnimationDefinition(chained: Boolean, curve: Compiler.AnimationCurve, duration: Int, delay: Int, components: Seq[Either[AnimationComponent, TempDefinition]])
  case class FunctionDeclaration(functionSignature: String, functionBody: String) extends Statement
  sealed trait StageComponent
  case class IntroDefinition(animations: Seq[AnimationDefinition]) extends StageComponent
  case class PeriodicPeriod(periodNum: Int, periodUnit: TimeUnit)
  case class PeriodicDefinition(period: PeriodicPeriod, animations: Seq[AnimationDefinition]) extends StageComponent
  case class ForeverComponent(signalName: String, value: String)
  case class TempDefinition(body: String)
  case class ForeverDefinition(components: Seq[Either[ForeverComponent, TempDefinition]]) extends StageComponent
  case class StageDefinition(stageComponents: Seq[StageComponent]) extends Statement
  case class TypeStructDeclaration(native: Boolean, fields: Seq[TypeStructField])
  case class TypeStructField(fieldName: String, typeName: String)
  case class TypeAliasDeclaration(aliasTo: String)

  def unordered[A](sep: Parser[_], min: Int, ps: Parser[A]*): Parser[Seq[A]] = {
    val sepNoCapture = sep >> ()
    ps.reduceLeft(_ | _).rep(min = min, sep = sepNoCapture)
  }

  val whiteSpace = P(CharsWhile(Character.isWhitespace))
  val wsp = whiteSpace
  val newline = "\n"
  val ident = P(CharIn('0' to '9', 'a' to 'z', 'A' to 'Z', "-_").rep(min = 1))
  val typeName = P(ident)
  val signalName = P(ident).log()
  val fieldName = P(ident)
  val layerName = P(ident)
  val animationName = P(ident)
  val tickTimeName = P(ident)
  val functionName = P(ident)
  val animationCurve: Parser[Compiler.AnimationCurve] = P(
    P("ease_in_out") >> EaseInOutCurve | P("linear") >> LinearCurve |
      P("ease_out") >> EaseOutCurve | P("ease_in") >> EaseInCurve
  )
  val timePeriod: Parser[Int] = P(CharIn("-").?.! ~ CharsWhile(Character.isDigit).! ~ "ms").map {
    case (neg, number) => (neg + number).toInt
  }
  val signalValue = P(CharPred(ch ⇒ ch != '}') | (CharIn("}") ~ !"}")).rep
  val signalDeclaration: Parser[SignalDeclaration] = P("signal" ~ wsp ~ typeName.! ~ wsp ~ signalName.! ~ wsp.? ~ ";" ~ newline).map {
    case (typeIdentifier, signalIdentifier) ⇒ SignalDeclaration(signalIdentifier = signalIdentifier, typeIdentifier = typeIdentifier)
  }
  val typeAliasDeclaration: Parser[TypeAliasDeclaration] = P(typeName.! ~ wsp.? ~ ";").map(TypeAliasDeclaration).log()
  val typeStructFieldDeclaration: Parser[TypeStructField] = P(typeName.! ~ wsp ~ fieldName.!).map(tup ⇒ TypeStructField.tupled(tup.swap)).log()
  val typeStructFields: Parser[Seq[TypeStructField]] = P(typeStructFieldDeclaration.rep(sep = wsp.? ~ "," ~ wsp.?, min = 1)).log()
  val typeStructDeclaration: Parser[TypeStructDeclaration] = P((P("native" ~ wsp) >> true | Pass >> false) ~ "{{" ~ wsp.? ~ typeStructFields ~ wsp.? ~ "}}").map(TypeStructDeclaration.tupled)
  val structOrAlias: Parser[Either[TypeStructDeclaration, TypeAliasDeclaration]] = P(typeStructDeclaration.map(Left.apply) | typeAliasDeclaration.map(Right.apply)).log()
  val typeDeclaration: Parser[TypeDeclaration] = P("type" ~ wsp ~ typeName.! ~ wsp.? ~ "=" ~ wsp.? ~ structOrAlias).map(TypeDeclaration.tupled).log()
  val drawProcDefinition: Parser[(String, String)] = P("(" ~ ident.! ~ ")" ~ wsp.? ~ "=>" ~ wsp.? ~ "{{" ~ signalValue.! ~ "}}")
  val layerDeclaration: Parser[Statement] = P("layer" ~ wsp ~ layerName.! ~ wsp.? ~ "=" ~ wsp.? ~ drawProcDefinition)
    .map(t => LayerDeclaration.tupled((t._1, t._2._1, t._2._2)))
  val constantDeclaration: Parser[Statement] = P(Fail)
  val optionalDelay: Parser[Option[Int]] = P("after" ~ wsp ~ timePeriod ~ wsp).?
  val animationInterpolated: Parser[AnimationComponent] = P(signalName.log().! ~ wsp.log() ~ "from".log() ~ wsp.log() ~ CharsWhile(_ != ';').log().! ~ wsp.?.log() ~ ";".log() ~ wsp.?.log() ~ "to".log() ~ wsp.log() ~ CharsWhile(_ != ';').!.log()).map(InterpolatedAnimationComponent.tupled).log()
  val animationConstant: Parser[AnimationComponent] = P(signalName.! ~ wsp.? ~ "=" ~ wsp.? ~ CharsWhile(_ != ';').!).map(ConstantAnimationComponent.tupled).log()
  val animationComponent: Parser[AnimationComponent] = P((animationInterpolated | animationConstant) ~ wsp.? ~ ";").log()
  val tempDefinition: Parser[TempDefinition] = P("temp" ~ wsp ~ (CharsWhile(_ != ';') ~ ";").!).map(TempDefinition).log()
  val animationDefinition: Parser[AnimationDefinition] = P(
    (("then" ~ wsp) >> true | Pass >> false) ~ animationCurve ~ wsp ~ "for" ~ wsp ~ timePeriod ~ wsp ~ optionalDelay ~
      wsp.? ~ "{{" ~ wsp.? ~ (animationComponent.map(Left(_)) | tempDefinition.map(Right(_))).rep(min = 0, sep = wsp.?) ~ wsp.? ~ "}}").map {
    case (chained, curve, duration, maybeDelay, components) => AnimationDefinition(chained, curve, duration, maybeDelay.getOrElse(0), components)
  }.log()
  val allAnimationDefinitions : Parser[Seq[AnimationDefinition]] = animationDefinition.rep(min = 1, sep = wsp.?)
  val introDefinition = P("intro" ~ wsp.? ~ "{{" ~ wsp.? ~ allAnimationDefinitions ~ wsp.? ~ "}}").map(IntroDefinition).log()
  val number = P(CharsWhile(CharPredicates.isDigit))
  val periodicTimeUnit: Parser[TimeUnit] = (P("minute" ~ "s".?) >> TimeUnit.MINUTES) | (P("hour" ~ "s".?) >> TimeUnit.HOURS)
  val periodicPeriod: Parser[PeriodicPeriod] = ((number.?.!.map(_.toInt) | Pass >> 1) ~ periodicTimeUnit).map(PeriodicPeriod.tupled)
  val periodicDefinition = P("every" ~ wsp ~ periodicPeriod ~ wsp.? ~ "{{" ~ wsp.? ~ allAnimationDefinitions ~ wsp.? ~ "}}").map(PeriodicDefinition.tupled).log()
  val foreverComponent = P(signalName.!.log() ~ wsp.?.log() ~ "=".log() ~ wsp.?.log() ~ CharsWhile(_ != ';').!.log() ~ wsp.?.log() ~ ";".log()).map(ForeverComponent.tupled).log()
  val foreverDefinition = P("forever" ~ wsp.? ~ "{{" ~ wsp.? ~ (foreverComponent.map(Left(_)) | tempDefinition.map(Right(_))).rep(min = 1, sep = wsp.?) ~ wsp.? ~ "}}").map(ForeverDefinition).log()
  val stageDefinition: Parser[StageDefinition] = P(unordered(wsp.?, 0, periodicDefinition, introDefinition, foreverDefinition)).map(StageDefinition(_)).log()
  val stageDeclaration: Parser[Statement] = P("stage" ~ wsp.? ~ "{{" ~ wsp.? ~ stageDefinition ~ wsp.? ~ "}}").log()
  val functionSignature = P(CharsWhile(ch ⇒ ch != '{')).log()
  val functionDeclaration: Parser[Statement] = P("function" ~ wsp ~ functionSignature.!.log() ~ wsp.?.log() ~ "{{" ~ signalValue.log().! ~ wsp.?.log() ~ "}}".log()).map(FunctionDeclaration.tupled)
  val statement: Parser[Statement] = P(signalDeclaration.log() | typeDeclaration.log() | layerDeclaration.log() | constantDeclaration.log() | stageDeclaration.log() | functionDeclaration.log())
  val program: Parser[Seq[Statement]] = P(Start ~ wsp.? ~ statement.rep(min = 1, sep = wsp.?) ~ wsp.? ~ End)
}
