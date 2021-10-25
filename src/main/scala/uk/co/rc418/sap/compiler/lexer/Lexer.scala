package uk.co.rc418.sap.compiler.lexer

import uk.co.rc418.sap.compiler.compiler.SapCompilationError

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object Lexer extends RegexParsers {
  override def skipWhitespace = false

  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-z][a-z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  def number: Parser[NUMBER] = positioned {
    "[0-9][0-9]*".r ^? { case number if Try(NUMBER(number.toInt)).isSuccess => NUMBER(number.toInt)}
  }

  private def let = positioned {
    "LET" ^^ (_ => LET())
  }

  private def goto = positioned {
    "GOTO" ^^ (_ => GOTO())
  }

  private def label = positioned {
    "LABEL" ^^ (_ => LABEL())
  }

  private def halt = positioned {
    "HALT" ^^ (_ => HALT())
  }

  private def output = positioned {
    "OUTPUT" ^^ (_ => OUTPUT())
  }

  private def equals = positioned {
    "=" ^^ (_ => EQUALS())
  }

  private def plus = positioned {
    "+" ^^ (_ => PLUS())
  }

  private def minus = positioned {
    "-" ^^ (_ => MINUS())
  }

  private def newline = positioned {
    "\n" ^^ (_ => NEWLINE())
  }

  private def eof = positioned {
    """\z""" ^^ (_ => EOF())
  }

  def whitespace: Parser[WHITESPACE] = positioned {
    "[ \t\r\f]+".r ^^ { _ => WHITESPACE() }
  }

  def tokens: Parser[List[SapToken]] = {
    phrase(rep1(let | goto | label | output | halt | equals | plus | minus | newline | identifier | number | eof | whitespace))
  }

  def apply(code: String): Either[SapCompilationError, List[SapToken]] = {
    parse(tokens, code) match {
      case NoSuccess(_, next) => Left(SapCompilationError(next.pos, "Invalid token found"))
      case Success(result, _) => Right(result)
    }
  }
}
