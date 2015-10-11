package io.signals

import java.util.concurrent.TimeUnit

import io.signals.Parsers._
import shapeless._
import scala.language.higherKinds
import scalaz.{Lens ⇒ _, _}
import Scalaz._

object Compiler {

  implicit class PartialFoldLeftSyntax[A](t: Seq[A]) {
    def partialFoldLeft[B: Monoid](pf: PartialFunction[A, B]): B = {
      t.foldLeft[B](Monoid[B].zero) { (acc: B, next: A) ⇒
        acc |+| pf.orElse[A, B] { case _ ⇒ Monoid[B].zero }.apply(next)
      }
    }
  }
  implicit def errorMonoid[A, B: Monoid]: Monoid[A \/ B] = new Monoid[A \/ B] {
    override def zero: A \/ B = Monoid[B].zero.right
    override def append(f1: A \/ B, f2: ⇒ A \/ B): A \/ B = for {
      x ← f1
      y ← f2
    } yield x |+| y
  }
  type Type = String

  sealed trait AnimationCurve
  case object EaseInOutCurve extends AnimationCurve
  case object EaseInCurve extends AnimationCurve
  case object EaseOutCurve extends AnimationCurve
  case object LinearCurve extends AnimationCurve

  case class Forever(components: Seq[ForeverComponent], temps: Seq[TempDefinition])

  case class DrawProc(code: String, contextName: String)
  case class StructDef(native: Boolean, fields: Seq[StructField])
  case class StructField(fieldName: String, typeName: Type)
  case class Signal(signalName: String, typeName: Type)
  case class Animation(durationMs: Int = 0,
                       delayMs: Int = 0,
                       curve: AnimationCurve = EaseInOutCurve,
                       components: Seq[AnimationComponent],
                       temps: Seq[TempDefinition])

  case class Understood(typeAliases: Map[Type, Type] = Map.empty, structDefinitions: Map[Type, StructDef] = Map.empty,
                        signals: Seq[Signal] = Seq.empty, layers: Map[String, DrawProc] = Map.empty, introAnimations: Seq[Animation] = Seq.empty,
                        periodicAnimations: Seq[(PeriodicPeriod, Seq[Animation])] = Seq.empty, forever: Forever = null, functions: Seq[FunctionDeclaration] = Seq.empty)

  sealed class CompilerError
  case object FirstAnimationChained extends CompilerError
  case object UndefinedSignalInAnimation extends CompilerError
  case object TooManyForevers extends CompilerError
  case object NoForever extends CompilerError
  case object NoLayers extends CompilerError
  case object TooManyIntros extends CompilerError

  val typeAliasLens: Lens[Understood, Map[Type, Type]] = lens[Understood] >> 'typeAliases
  val structDefinitionLens: Lens[Understood, Map[Type, StructDef]] = lens[Understood] >> 'structDefinitions
  val signalLens: Lens[Understood, Seq[Signal]] = lens[Understood] >> 'signals
  val layersLens: Lens[Understood, Map[String, DrawProc]] = lens[Understood] >> 'layers
  val introAnimationLens: Lens[Understood, Seq[Animation]] = lens[Understood] >> 'introAnimations
  val foreverLens: Lens[Understood, Forever] = lens[Understood] >> 'forever
  val functionLens: Lens[Understood, Seq[FunctionDeclaration]] = lens[Understood] >> 'functions

  implicit object UnderstoodSemigroup extends Semigroup[Understood] {
    override def append(f1: Understood, f2: ⇒ Understood): Understood = f1
  }

  def verifyIntroAnimations(understood: Understood): ValidationNel[CompilerError, Understood] = {
    understood.introAnimations.flatMap(_.components.map {
      case InterpolatedAnimationComponent(signal, _, _) ⇒
        understood.successNel.ensure(UndefinedSignalInAnimation.wrapNel)(_.signals.exists(_.signalName == signal))
      case ConstantAnimationComponent(signal, _) ⇒
        understood.successNel.ensure(UndefinedSignalInAnimation.wrapNel)(_.signals.exists(_.signalName == signal))
    }).reduce(_ +++ _)
  }

  def verifyEverythingDefined(understood: Understood): ValidationNel[CompilerError, Understood] = {
    understood.successNel.ensure(NoForever.wrapNel)(_.forever != null) +++
      understood.successNel.ensure(NoLayers.wrapNel)(_.layers.nonEmpty)
  }

  def translateAnimations(animationDefinitions: Seq[AnimationDefinition]): CompilerError \/ Seq[Animation] = {
    def tryToChain(animsSoFar: Seq[Animation], newAnim: AnimationDefinition): CompilerError \/ Animation = {
      val delayWithErr =
        if (newAnim.chained) {
          animsSoFar.headOption.fold[CompilerError \/ Int](FirstAnimationChained.left)(lastAnim ⇒
            (newAnim.delay + lastAnim.delayMs + lastAnim.durationMs).right)
        } else {
          newAnim.delay.right[CompilerError]
        }
      delayWithErr.map { delay ⇒
        Animation(newAnim.duration, delay, newAnim.curve, newAnim.components.flatMap(_.left.toOption),
          newAnim.components.flatMap(_.right.toOption))
      }
    }
    animationDefinitions.foldLeft(Seq[Animation]().right[CompilerError]) { (soFar, newAnimDef) ⇒
      for {
        animsSoFar ← soFar
        newAnim ← tryToChain(animsSoFar, newAnimDef)
      } yield newAnim +: animsSoFar
    }
  }

  def understand(program: Seq[Parsers.Statement]): ValidationNel[CompilerError, Understood] = {
    val compiled = program.foldLeft(Understood().right[CompilerError]) { (soFar, newStatement) ⇒
      soFar.flatMap { understoodSoFar ⇒
        newStatement match {
          case Parsers.SignalDeclaration(signalName, typeName) ⇒
            signalLens.modify(understoodSoFar)(ss ⇒ Signal(signalName, typeName) +: ss).right
          case Parsers.TypeDeclaration(typeName, structOrAlias) ⇒
            structOrAlias.fold({ structDecl ⇒
              structDefinitionLens.modify(understoodSoFar)(us ⇒ us + (typeName → StructDef(structDecl.native, structDecl.fields.map(f ⇒ StructField(f.fieldName, f.typeName))))).right
            }, { aliasDecl ⇒
              typeAliasLens.modify(understoodSoFar)(as ⇒ as + (typeName → aliasDecl.aliasTo)).right
            })
          case Parsers.LayerDeclaration(layerName, contextName, code) ⇒
            layersLens.modify(understoodSoFar)(as ⇒ as + (layerName → DrawProc(code, contextName))).right
          case Parsers.StageDefinition(stageComponents) ⇒
            val numForevers = stageComponents.count(_.isInstanceOf[ForeverDefinition])
            val numIntros = stageComponents.count(_.isInstanceOf[IntroDefinition])
            if (numIntros > 1) {
              TooManyIntros.left
            } else if (numForevers > 1) {
              TooManyForevers.left
            } else if (numForevers == 0) {
              NoForever.left
            } else {
              val introAnimationsWithErr = stageComponents.partialFoldLeft {
                case IntroDefinition(animations) ⇒ translateAnimations(animations).map(_.toVector)
              }
              val forever = stageComponents.partialFoldLeft {
                case ForeverDefinition(components) ⇒
                  Vector(Forever(components.flatMap(_.left.toOption), components.flatMap(_.right.toOption)))
              }.head
              val periodicAnimationsWithErr = stageComponents.partialFoldLeft {
                case PeriodicDefinition(period, periodAnimations) ⇒ translateAnimations(periodAnimations).map { animations ⇒
                  Vector((period, animations))
                }
              }
              for {
                introAnimations ← introAnimationsWithErr
                periodicAnimations ← periodicAnimationsWithErr
              } yield understoodSoFar.copy(
                introAnimations = introAnimations,
                periodicAnimations = periodicAnimations,
                forever = forever
              )
            }
          case decl@Parsers.FunctionDeclaration(_, _) ⇒
            functionLens.modify(understoodSoFar)(decl +: _).right
        }
      }
    }.validation.leftMap(_.wrapNel)
    compiled +++ compiled.flatMap(verifyIntroAnimations) +++ compiled.flatMap(verifyEverythingDefined)
  }
}
