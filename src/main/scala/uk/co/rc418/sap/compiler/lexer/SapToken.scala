package uk.co.rc418.sap.compiler.lexer

import scala.util.parsing.input.Positional

sealed trait SapToken extends Positional

case class LABEL() extends SapToken
case class GOTO() extends SapToken
case class OUTPUT() extends SapToken
case class LET() extends SapToken
case class HALT() extends SapToken

case class EQUALS() extends SapToken
case class PLUS() extends SapToken
case class MINUS() extends SapToken

case class EOF() extends SapToken
case class NEWLINE() extends SapToken

case class NUMBER(number: Int) extends SapToken {
  require(number > 0 && number < 256)
}

case class IDENTIFIER(id: String) extends SapToken

case class WHITESPACE() extends SapToken
