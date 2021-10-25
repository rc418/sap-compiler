package uk.co.rc418.sap.compiler.compiler

import scala.language.implicitConversions
import scala.util.parsing.input.Position

case class SapCompilationError(location: Position, msg: String)

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

object Location {
  implicit def fromPos(pos: Position): Location = Location(pos.line, pos.column)
}
