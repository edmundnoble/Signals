package io.rxpebble

import shapeless._

object Lexer {
  type Type = String

  case class StructField(fieldName: String, typeName: Type)

  case class Signal(signalName: String, typeName: Type)

  case class Understood(typeAliases: Map[Type, Type], structDefinitions: Map[Type, Seq[StructField]],
                        signals: Seq[Signal])

  val typeAliasLens: Lens[Understood, Map[Type, Type]] = lens[Understood] >> 'typeAliases
  val structDefinitionLens: Lens[Understood, Map[Type, Seq[StructField]]] = lens[Understood] >> 'structDefinitions
  val signalLens: Lens[Understood, Seq[Signal]] = lens[Understood] >> 'signals

  def understand(program: Seq[Parsers.Statement]): Understood = {
    program.foldLeft(Understood(Map(), Map(), Seq())) { (understoodSoFar, newStatement) =>
      newStatement match {
        case Parsers.SignalDeclaration(signalName, typeName) => signalLens.modify(understoodSoFar)(ss => Signal(signalName, typeName) +: ss)
        case Parsers.TypeDeclaration(typeName, structOrAlias) => structOrAlias.fold({ structDecl =>
          structDefinitionLens.modify(understoodSoFar)(us => us + (typeName -> structDecl.fields.map(f => StructField(f.fieldName, f.typeName))))
        }, { aliasDecl =>
          typeAliasLens.modify(understoodSoFar)(as => as + (typeName -> aliasDecl.aliasTo))
        })
      }
    }
  }
}
