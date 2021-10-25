package uk.co.rc418.sap.compiler.asm

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.co.rc418.sap.compiler.lexer.Lexer
import uk.co.rc418.sap.compiler.parser.{SapAST, SapParser}

class SapCompilerSpec extends AnyWordSpec with Matchers {



  private def runCompiler(code: String) = {
    AsmCompiler(SapParser(Lexer(code).getOrElse(Seq())).getOrElse(null))
  }
}
