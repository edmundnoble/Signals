package io.signals

import io.signals.Parsers._
import shapeless._
import scalaz.{Lens ⇒ _, _}
import Scalaz._

object Compiler {
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
                        signals: Seq[Signal] = Seq.empty, layers: Map[String, DrawProc] = Map.empty, animations: Seq[Animation] = Seq.empty,
                        forever: Forever = null, functions: Seq[FunctionDeclaration] = Seq.empty)

  sealed class CompilerError
  val FirstAnimationChained = new CompilerError {}
  val UndefinedSignalInAnimation = new CompilerError {}

  val typeAliasLens: Lens[Understood, Map[Type, Type]] = lens[Understood] >> 'typeAliases
  val structDefinitionLens: Lens[Understood, Map[Type, StructDef]] = lens[Understood] >> 'structDefinitions
  val signalLens: Lens[Understood, Seq[Signal]] = lens[Understood] >> 'signals
  val layersLens: Lens[Understood, Map[String, DrawProc]] = lens[Understood] >> 'layers
  val animationLens: Lens[Understood, Seq[Animation]] = lens[Understood] >> 'animations
  val foreverLens: Lens[Understood, Forever] = lens[Understood] >> 'forever
  val functionLens: Lens[Understood, Seq[FunctionDeclaration]] = lens[Understood] >> 'functions

  implicit object UnderstoodSemigroup extends Semigroup[Understood] {
    override def append(f1: Understood, f2: ⇒ Understood): Understood = f1
  }

  def verifyAnimations(understood: Understood): ValidationNel[CompilerError, Understood] = {
    understood.animations.flatMap(_.components.map {
      case InterpolatedAnimationComponent(signal, _, _) ⇒
        understood.successNel.ensure(UndefinedSignalInAnimation.wrapNel)(_.signals.exists(_.signalName == signal))
      case ConstantAnimationComponent(signal, _) ⇒
        understood.successNel.ensure(UndefinedSignalInAnimation.wrapNel)(_.signals.exists(_.signalName == signal))
    }).reduce(_ +|+ _)
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
          case Parsers.StageDefinition(animationDefinitions, forever) ⇒
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
            val animationsWithErr = animationDefinitions.foldLeft[CompilerError \/ Seq[Animation]](Seq[Animation]().right[CompilerError]) { (soFar, newAnimDef) ⇒
              for {
                animsSoFar ← soFar
                newAnim ← tryToChain(animsSoFar, newAnimDef)
              } yield newAnim +: animsSoFar
            }
            println(forever.components)
            animationsWithErr.map { animations ⇒
              understoodSoFar.copy(
                animations = animations,
                forever = Forever(forever.components.flatMap(_.left.toOption), forever.components.flatMap(_.right.toOption))
              )
            }
          case decl@Parsers.FunctionDeclaration(_, _) ⇒
            functionLens.modify(understoodSoFar)(decl +: _).right
        }
      }
    }.validation.leftMap(_.wrapNel)
    compiled +|+ compiled.flatMap(verifyAnimations)
  }
}
