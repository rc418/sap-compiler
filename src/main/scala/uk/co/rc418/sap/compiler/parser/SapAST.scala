package uk.co.rc418.sap.compiler.parser

import scala.util.parsing.input.{Position, Positional}

sealed trait SapAST extends Positional

case class DefineVariable(id: Identifier, num: Int) extends SapAST
case class AssignNumber(id: Identifier, num: Int) extends SapAST
case class AssignVariable(id0: Identifier, id1: Identifier) extends SapAST
case class AssignExpression(id: Identifier, expression: ExpressionAST) extends SapAST
case class Label(name: String) extends SapAST
case class Goto(label: String) extends SapAST
case class Output(id: Identifier) extends SapAST
case class Halt() extends SapAST
case class Identifier(id: String) extends SapAST

case class AndThen(step1: SapAST, step2: SapAST) extends SapAST

sealed trait ExpressionAST extends SapAST
case class Addition(parameters: ParametersAST) extends ExpressionAST
case class Subtraction(parameters: ParametersAST) extends ExpressionAST

sealed trait ParametersAST extends SapAST

case class IdToId(id1: Identifier, id2: Identifier) extends ParametersAST
case class IdToNumber(id: Identifier, num: Int) extends ParametersAST
case class NumberToId(num: Int, id: Identifier) extends ParametersAST
case class NumberToNumber(num1: Int, num2: Int) extends ParametersAST

object Goto {
  def apply(label :String, position: Position): Goto = {
    val g = Goto(label)
    g.setPos(position)
    g
  }
}

object Identifier {
  def apply(id :String, position: Position): Identifier = {
    val i = Identifier(id)
    i.setPos(position)
    i
  }
}