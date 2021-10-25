package uk.co.rc418.sap.compiler.parser

import uk.co.rc418.sap.compiler.compiler.SapCompilationError

object SapASTValidator {

  def apply(ast: SapAST): Either[SapCompilationError, SapAST] = {
    for {
      g <- checkGotos(ast)
      v <- checkVariables(g)
    } yield v
  }

  private def checkGotos(ast: SapAST): Either[SapCompilationError, SapAST] = {
    checkGotos(ast, findAllLabels(ast))
  }
  private def checkGotos(ast: SapAST, labels: Seq[String]): Either[SapCompilationError, SapAST] = {
    val result = ast match {
      case Goto(label) if !labels.contains(label) => Left(SapCompilationError(ast.pos, s"GOTO found without corresponding LABEL($label)"))
      case AndThen(step1, step2) => step1 match {
        case Goto(label) if !labels.contains(label) => Left(SapCompilationError(step1.pos, s"GOTO found without corresponding LABEL($label)"))
        case _ => checkGotos(step2, labels)
      }
      case _ => Right(ast)
    }
    if (result.isLeft) result else Right(ast)
  }

  private def findAllLabels(ast: SapAST): Seq[String] = {
    Seq() ++ {
      ast match {
        case Label(name) => Seq(name)
        case AndThen(step1, step2) => findAllLabels(step1) ++ findAllLabels(step2)
        case _ => Seq()
      }
    }
  }

  def checkVariablesDefinedFirst(ast: SapAST, nonDefineFound: Boolean = false): Either[SapCompilationError, Boolean] =
    ast match {
      case DefineVariable(id, _) if nonDefineFound => {
        Left(SapCompilationError(id.pos, s"Variable definition (${id.id}) not at top of program"))
      }
      case DefineVariable(_, _) => Right(nonDefineFound)
      case AndThen(step1, step2) => checkVariablesDefinedFirst(step1, nonDefineFound).flatMap(found => checkVariablesDefinedFirst(step2, found))
      case _ => Right(true)
    }

  private def checkVariables(ast: SapAST, identifiers: Seq[String] = Seq()): Either[SapCompilationError, SapAST] = {
    checkVariablesDefinedFirst(ast).flatMap(_ => {
      val result = ast match {
        case DefineVariable(id, _) if identifiers.contains(id.id) => identifierAlreadyDefinedError(id)
        case AssignNumber(id, _) if !identifiers.contains(id.id) => identifierError(id)
        case AssignVariable(id, _) if !identifiers.contains(id.id) => identifierError(id)
        case AssignVariable(_, id) if !identifiers.contains(id.id) => identifierError(id)
        case AssignExpression(id, _) if !identifiers.contains(id.id) => identifierError(id)
        case AssignExpression(_, Addition(parameters)) => checkParameters(parameters, identifiers)
        case AssignExpression(_, Subtraction(parameters)) => checkParameters(parameters, identifiers)
        case Output(id) if !identifiers.contains(id.id) => identifierError(id)
        case AndThen(step1, step2) => step1 match {
          case DefineVariable(id, _) if identifiers.contains(id.id) => identifierAlreadyDefinedError(id)
          case DefineVariable(id, _) => checkVariables(step2, identifiers :+ id.id)
          case AssignNumber(id, _) if !identifiers.contains(id.id) => identifierError(id)
          case AssignVariable(id, _) if !identifiers.contains(id.id) => identifierError(id)
          case AssignVariable(_, id) if !identifiers.contains(id.id) => identifierError(id)
          case AssignExpression(id, _) if !identifiers.contains(id.id) => identifierError(id)
          case AssignExpression(_, Addition(parameters)) => checkParameters(parameters, identifiers)
          case AssignExpression(_, Subtraction(parameters)) => checkParameters(parameters, identifiers)
          case Output(id) if !identifiers.contains(id.id) => identifierError(id)
          case _ => checkVariables(step2, identifiers)
        }
        case _ => Right(ast)
      }
      result.flatMap(_ => Right(ast))
    })
  }

  private def checkParameters(parameters: ParametersAST, identifiers: Seq[String]): Either[SapCompilationError, SapAST] ={
    parameters match {
      case IdToId(id, _) if !identifiers.contains(id.id) => identifierError(id)
      case IdToId(_, id) if !identifiers.contains(id.id) => identifierError(id)
      case IdToNumber(id, _) if !identifiers.contains(id.id) => identifierError(id)
      case NumberToId(_, id) if !identifiers.contains(id.id) => identifierError(id)
      case _ => Right(parameters)
    }
  }

  private def identifierError(id: Identifier): Left[SapCompilationError, SapAST] = {
    Left(SapCompilationError(id.pos, s"Identifier (${id.id}) used without being defined"))
  }

  private def identifierAlreadyDefinedError(id: Identifier): Left[SapCompilationError, SapAST] = {
    Left(SapCompilationError(id.pos, s"Identifier (${id.id}) is already defined"))
  }
}
