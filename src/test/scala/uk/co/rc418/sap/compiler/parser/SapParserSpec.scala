package uk.co.rc418.sap.compiler.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.co.rc418.sap.compiler.lexer.Lexer

import util.parsing.input.NoPosition

class SapParserSpec extends AnyWordSpec with Matchers {

  "Parser" when {
    "passed valid code" should {
      "parse valid program 1" in {
        val validCode = "LET a = 5\nLET b = 5\nLABEL start\na = a + b\nGOTO start\n"
        val successfulAST =
          AndThen(
            DefineVariable(Identifier("a", NoPosition), 5),
            AndThen(
              DefineVariable(Identifier("b", NoPosition), 5),
              AndThen(
                Label("start"),
                AndThen(AssignExpression(Identifier("a", NoPosition), Addition(IdToId(Identifier("a", NoPosition), Identifier("b", NoPosition)))),
                  Goto("start")))))
        runParser(validCode) shouldBe Right(successfulAST)
      }
      "parse valid program 2" in {
        val validCode = "LET a = 5\nOUTPUT a\nHALT\n"
        val successfulAST =
          AndThen(
            DefineVariable(Identifier("a", NoPosition), 5),
            AndThen(
              Output(Identifier("a", NoPosition)),
              Halt()))
        runParser(validCode) shouldBe Right(successfulAST)
      }
    }
    "passed invalid code" should {
      "Missing variable declaration" in {
        val inValidCode = "LET a = 5\nLABEL start\na = a + b\n"
        val result = runParser(inValidCode)
        result match {
          case Left(value) => value.msg shouldBe "Identifier (b) used without being defined"
          case Right(_) => fail()
        }
      }
      "Duplicated LET statement" in {
        val inValidCode = "LET LET a = 5\nLET b = 5\nLABEL start\na = a + b\nGOTO start\n"
        val result = runParser(inValidCode)
        result match {
          case Left(value) => value.msg shouldBe "identifier expected"
          case Right(_) => fail()
        }
      }
      "Variable definitions must be at top of program" in {
        val inValidCode = "LET a = 5\nGOTO start\nLET b = 5\nLABEL start\na = a + b\n"
        val result = runParser(inValidCode)
        println(result)
        result match {
          case Left(value) => value.msg shouldBe "Variable definition (b) not at top of program"
          case Right(_) => fail()
        }
      }
    }
  }

  private def runParser(code: String) = {
    SapParser(Lexer(code).getOrElse(Seq()))
  }
}
