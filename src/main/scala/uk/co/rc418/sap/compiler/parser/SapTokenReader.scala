package uk.co.rc418.sap.compiler.parser

import uk.co.rc418.sap.compiler.lexer.SapToken

import scala.util.parsing.input.{NoPosition, Position, Reader}

class SapTokenReader(tokens: Seq[SapToken]) extends Reader[SapToken] {
  override def first: SapToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = if (tokens.isEmpty) NoPosition else first.pos
  override def rest: Reader[SapToken] = new SapTokenReader(tokens.tail)
}