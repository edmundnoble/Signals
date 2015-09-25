package io.rxpebble

import shapeless._

object Lexer {
  type Type = String

  sealed trait AnimationCurve
  case object EaseInOutCurve extends AnimationCurve
  case object EaseInCurve extends AnimationCurve
  case object EaseOutCurve extends AnimationCurve
  case object LinearCurve extends AnimationCurve

  case class DrawProc(code: String, contextName: String)
  case class StructField(fieldName: String, typeName: Type)
  case class Signal(signalName: String, typeName: Type)
  case class AnimationComponent(signalName: String, startValue: String, endValue: String)
  case class Animation(durationMs: Int = 0,
                       delayMs: Int = 0,
                       curve: AnimationCurve = EaseInOutCurve,
                       components: Seq[AnimationComponent])

  case class Understood(typeAliases: Map[Type, Type] = Map.empty, structDefinitions: Map[Type, Seq[StructField]] = Map.empty,
                        signals: Seq[Signal] = Seq.empty, layers: Map[String, DrawProc] = Map.empty, animations: Seq[Animation] = Seq.empty)

  val typeAliasLens: Lens[Understood, Map[Type, Type]] = lens[Understood] >> 'typeAliases
  val structDefinitionLens: Lens[Understood, Map[Type, Seq[StructField]]] = lens[Understood] >> 'structDefinitions
  val signalLens: Lens[Understood, Seq[Signal]] = lens[Understood] >> 'signals
  val layersLens: Lens[Understood, Map[String, DrawProc]] = lens[Understood] >> 'layers
  val animationLens: Lens[Understood, Seq[Animation]] = lens[Understood] >> 'animations

  def understand(program: Seq[Parsers.Statement]): Understood = {
    // TODO: Avoid hardcoding clock_layer in
    program.foldLeft(Understood()) { (understoodSoFar, newStatement) =>
      newStatement match {
        case Parsers.SignalDeclaration(signalName, typeName) => signalLens.modify(understoodSoFar)(ss => Signal(signalName, typeName) +: ss)
        case Parsers.TypeDeclaration(typeName, structOrAlias) => structOrAlias.fold({ structDecl =>
          structDefinitionLens.modify(understoodSoFar)(us => us + (typeName -> structDecl.fields.map(f => StructField(f.fieldName, f.typeName))))
        }, { aliasDecl =>
          typeAliasLens.modify(understoodSoFar)(as => as + (typeName -> aliasDecl.aliasTo))
        })
        case Parsers.LayerDeclaration(layerName, contextName, code) =>
          layersLens.modify(understoodSoFar)(as => as + (layerName -> DrawProc(code, contextName)))
        case Parsers.AnimationDeclaration(signalName, fromValue, toValue) =>
          animationLens.modify(understoodSoFar)(as => Animation(Seq(AnimationComponent(signalName, fromValue, toValue))) +: as)
      }
    }
  }
}
