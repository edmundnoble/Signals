package io.signals

import java.io.{PrintWriter, File}
import java.nio.file.{FileAlreadyExistsException, Files, Paths}

import fastparse.core.Result

import scala.io.Source
import scalaz.{Success, Failure}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      throw new IllegalArgumentException("Not enough arguments!")
    }
    val programText = if (new File("prelude.rxp").exists()) {
      Source.fromFile("prelude.rxp").mkString + "\n" + Source.fromFile(args(0)).mkString
    } else {
      Source.fromFile(args(0)).mkString
    }
    val targetPath = Paths.get(args(1))
    try { Files.createDirectory(targetPath) } catch { case _: FileAlreadyExistsException ⇒ }
    val programParseResult = Parsers.program.parse(programText)
    val compiled = Compiler.understand(programParseResult.get.value) match {
      case Failure(failures) ⇒ println(failures); sys.exit(1)
      case Success(result) ⇒ result
    }
    val source = Output.mkProgram(compiled)
    source.foreach {
      case (fileName, code) ⇒
        val writer = new PrintWriter(new File(args(1) + File.separator + fileName))
        writer.write(code)
        writer.close()
    }
  }
}
