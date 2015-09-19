package io.rxpebble

import fastparse.core.Result

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      throw new IllegalArgumentException("Not enough arguments!")
    }
    val programText = Source.fromFile(args(0)).mkString
    val programParseResult = Parsers.program.parse(programText)
    println(Lexer.understand(programParseResult.get.value))
  }
}
