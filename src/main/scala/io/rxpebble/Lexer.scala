package io.rxpebble

import shapeless._

object Lexer {
  type Type = String

  case class DrawProc(code: String, contextName: String)
  case class StructField(fieldName: String, typeName: Type)
  case class Signal(signalName: String, typeName: Type)

  case class Understood(typeAliases: Map[Type, Type], structDefinitions: Map[Type, Seq[StructField]],
                        signals: Seq[Signal], layers: Map[String, DrawProc])

  val typeAliasLens: Lens[Understood, Map[Type, Type]] = lens[Understood] >> 'typeAliases
  val structDefinitionLens: Lens[Understood, Map[Type, Seq[StructField]]] = lens[Understood] >> 'structDefinitions
  val signalLens: Lens[Understood, Seq[Signal]] = lens[Understood] >> 'signals
  val layersLens: Lens[Understood, Map[String, DrawProc]] = lens[Understood] >> 'layers

  def understand(program: Seq[Parsers.Statement]): Understood = {
    // TODO: Avoid hardcoding clock_layer in
    program.foldLeft(Understood(Map(), Map(), Seq(), Map())) { (understoodSoFar, newStatement) =>
      newStatement match {
        case Parsers.SignalDeclaration(signalName, typeName) => signalLens.modify(understoodSoFar)(ss => Signal(signalName, typeName) +: ss)
        case Parsers.TypeDeclaration(typeName, structOrAlias) => structOrAlias.fold({ structDecl =>
          structDefinitionLens.modify(understoodSoFar)(us => us + (typeName -> structDecl.fields.map(f => StructField(f.fieldName, f.typeName))))
        }, { aliasDecl =>
          typeAliasLens.modify(understoodSoFar)(as => as + (typeName -> aliasDecl.aliasTo))
        })
        case Parsers.LayerDeclaration(layerName, contextName, code) =>
          layersLens.modify(understoodSoFar)(as => as + (layerName -> DrawProc(code, contextName)))
      }
    }
  }
}
