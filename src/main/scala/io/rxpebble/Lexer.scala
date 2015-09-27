package io.rxpebble

import io.rxpebble.Parsers.{ForeverDefinition, AnimationDefinition}
import shapeless._
import scalaz.{Lens ⇒ _, _}
import Scalaz._

object Lexer {
  type Type = String

  sealed trait AnimationCurve
  case object EaseInOutCurve extends AnimationCurve
  case object EaseInCurve extends AnimationCurve
  case object EaseOutCurve extends AnimationCurve
  case object LinearCurve extends AnimationCurve

  case class Forever(timeName: String, code: String)

  case class DrawProc(code: String, contextName: String)
  case class StructField(fieldName: String, typeName: Type)
  case class Signal(signalName: String, typeName: Type)
  case class AnimationComponent(signalName: String, startValue: String, endValue: String)
  case class Animation(durationMs: Int = 0,
                       delayMs: Int = 0,
                       curve: AnimationCurve = EaseInOutCurve,
                       components: Seq[AnimationComponent])

  case class Understood(typeAliases: Map[Type, Type] = Map.empty, structDefinitions: Map[Type, Seq[StructField]] = Map.empty,
                        signals: Seq[Signal] = Seq.empty, layers: Map[String, DrawProc] = Map.empty, animations: Seq[Animation] = Seq.empty,
                        forever: Forever = null)

  sealed trait LexerError
  case object FirstAnimationChained extends LexerError

  val typeAliasLens: Lens[Understood, Map[Type, Type]] = lens[Understood] >> 'typeAliases
  val structDefinitionLens: Lens[Understood, Map[Type, Seq[StructField]]] = lens[Understood] >> 'structDefinitions
  val signalLens: Lens[Understood, Seq[Signal]] = lens[Understood] >> 'signals
  val layersLens: Lens[Understood, Map[String, DrawProc]] = lens[Understood] >> 'layers
  val animationLens: Lens[Understood, Seq[Animation]] = lens[Understood] >> 'animations
  val foreverLens: Lens[Understood, Forever] = lens[Understood] >> 'forever

  def understand(program: Seq[Parsers.Statement]): LexerError \/ Understood = {
    program.foldLeft(Understood().right[LexerError]) { (soFar, newStatement) ⇒
      soFar.flatMap { understoodSoFar ⇒
        newStatement match {
          case Parsers.SignalDeclaration(signalName, typeName) ⇒
            signalLens.modify(understoodSoFar)(ss ⇒ Signal(signalName, typeName) +: ss).right[LexerError]
          case Parsers.TypeDeclaration(typeName, structOrAlias) ⇒
            structOrAlias.fold({ structDecl ⇒
              structDefinitionLens.modify(understoodSoFar)(us ⇒ us + (typeName → structDecl.fields.map(f ⇒ StructField(f.fieldName, f.typeName)))).right[LexerError]
            }, { aliasDecl ⇒
              typeAliasLens.modify(understoodSoFar)(as ⇒ as + (typeName → aliasDecl.aliasTo)).right[LexerError]
            })
          case Parsers.LayerDeclaration(layerName, contextName, code) ⇒
            layersLens.modify(understoodSoFar)(as ⇒ as + (layerName → DrawProc(code, contextName))).right[LexerError]
          case Parsers.StageDefinition(animationDefinitions, forever) ⇒
            def tryToChain(animsSoFar: Seq[Animation], newAnim: AnimationDefinition): LexerError \/ Animation = {
              val delayWithErr =
                if (newAnim.chained) {
                  animsSoFar.headOption.fold[LexerError \/ Int](FirstAnimationChained.left)(lastAnim ⇒
                    (newAnim.delay + lastAnim.delayMs + lastAnim.durationMs).right)
                } else {
                  newAnim.delay.right[LexerError]
                }
              delayWithErr.map { delay ⇒
                Animation(newAnim.duration, delay, newAnim.curve, newAnim.components.map(comp ⇒ AnimationComponent.tupled(Parsers.AnimationComponent.unapply(comp).get)))
              }
            }
            val animationsWithErr = animationDefinitions.foldLeft[LexerError \/ Seq[Animation]](Seq[Animation]().right[LexerError]) { (soFar, newAnimDef) ⇒
              for {
                animsSoFar ← soFar
                newAnim ← tryToChain(animsSoFar, newAnimDef)
              } yield newAnim +: animsSoFar
            }
            animationsWithErr.map { animations ⇒
              understoodSoFar.copy(animations = animations, forever = Forever.tupled(ForeverDefinition.unapply(forever).get))
            }
        }
      }
    }
  }
}
