package uk.co.rc418.sap.compiler.compiler

import uk.co.rc418.sap.compiler.asm.{AsmCompiler, AsmToken}
import uk.co.rc418.sap.compiler.lexer.Lexer
import uk.co.rc418.sap.compiler.parser.SapParser

object Compiler {
  def apply(code: String): Either[SapCompilationError, Seq[AsmToken]] = {
    for {
      tokens <- Lexer(if(code.endsWith("\n")) code else code + "\n")
      ast <- SapParser(tokens)
      assembly <- Right(AsmCompiler(ast))
    } yield assembly
  }
}
