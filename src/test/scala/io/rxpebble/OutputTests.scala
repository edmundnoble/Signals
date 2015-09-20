package io.rxpebble

import org.scalatest.{Matchers, WordSpec}


class OutputTests extends WordSpec with Matchers {
  "A struct" should {
    "have correct definitions generated" in {
      val correct =
        """typedef struct type {
          |  Layer layer;
          |} type;""".stripMargin
      Output.generateDefinition("type", Seq(Lexer.StructField("layer", "Layer"))) should be(correct)
    }
    "have correct declarations generated" in {
      val correct =
        """struct type;
          |typedef struct type type;""".stripMargin
      Output.generateDeclaration("type", Seq(Lexer.StructField("layer", "Layer"))) should be(correct)
    }
  }
  "An alias" should {
    "have correct declarations generated" in {
      val correct =
        """typedef type_from type_to;""".stripMargin
      Output.generateAlias("type_from", "type_to") should be(correct)
    }
  }
}
