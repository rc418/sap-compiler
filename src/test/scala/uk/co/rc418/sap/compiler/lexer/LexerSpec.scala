package uk.co.rc418.sap.compiler.lexer

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LexerSpec extends AnyWordSpec with Matchers {

  "Lexer" when {
    "passed valid token" should {
      "LABEL return LABEL" in {
        val result = Lexer("LABEL").getOrElse(List())
        result.length shouldBe 1
        result.head shouldBe LABEL()
      }
      "GOTO return GOTO" in {
        val result = Lexer("GOTO").getOrElse(List())
        result.length shouldBe 1
        result.head shouldBe GOTO()
      }
      "OUTPUT return OUTPUT" in {
        val result = Lexer("OUTPUT").getOrElse(List())
        result.length shouldBe 1
        result.head shouldBe OUTPUT()
      }
      "LET return let" in {
        val result = Lexer("LET").getOrElse(List())
        result.length shouldBe 1
        result.head shouldBe LET()
      }
      "HALT return HALT" in {
        val result = Lexer("HALT").getOrElse(List())
        result.length shouldBe 1
        result.head shouldBe HALT()
      }
      "= return EQUALS" in {
        val result = Lexer("=").getOrElse(List())
        result.length shouldBe 1
        result.head shouldBe EQUALS()
      }
      "+ return PLUS" in {
        val result = Lexer("+").getOrElse(List())
        result.length shouldBe 1
        result.head shouldBe PLUS()
      }
      "- return MINUS" in {
        val result = Lexer("-").getOrElse(List())
        result.length shouldBe 1
        result.head shouldBe MINUS()
      }
      "\\n return NEWLINE" in {
        val result = Lexer("\nLET").getOrElse(List()) //Last newline in input is stripped
        result.length shouldBe 2
        result.head shouldBe NEWLINE()
      }
      "42 return NUMBER" in {
        val result = Lexer("42").getOrElse(List())
        result.length shouldBe 1
        result.head shouldBe NUMBER(42)
      }
      "large numbers error" in {
        val result = Lexer("256")
        result.isLeft shouldBe true
      }
      "an identifier return IDENTIFIER" in {
        val result = Lexer("x").getOrElse(List())
        result.length shouldBe 1
        result.head shouldBe IDENTIFIER("x")
      }
    }
    "passed an invalid token" should {
      "error" in {
        val result = Lexer("INVALID_TOKEN")
        result.isLeft shouldBe true
      }
    }
    "passed valid tokens" should {
      "in correct order" in {
        val result = Lexer("LET a = 1\nLET b = 2\na = a - b\nOUTPUT\nHALT").getOrElse(List())
        result.length shouldBe 29
      }
      "in invalid order" in { // LEXER cares not for program logic
        val result = Lexer("LET a = 1\na = a - b\nOUTPUT\nHALT\nLET b = 2").getOrElse(List())
        result.length shouldBe 29
      }
    }
  }
}