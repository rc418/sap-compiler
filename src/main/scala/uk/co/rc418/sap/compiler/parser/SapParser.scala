package uk.co.rc418.sap.compiler.parser

import uk.co.rc418.sap.compiler.compiler.SapCompilationError
import uk.co.rc418.sap.compiler.lexer._

import scala.util.parsing.combinator.Parsers

object SapParser extends Parsers {
  override type Elem = SapToken


  def apply(tokens: Seq[SapToken]): Either[SapCompilationError, SapAST] = {
    val reader = new SapTokenReader(tokens.filter(p => !p.isInstanceOf[WHITESPACE]))
    for {
      parsed <- program(reader) match {
        case NoSuccess (msg, next) => Left (SapCompilationError(next.pos, msg))
        case Success (result, _) => Right (result)
      }
      validated <- SapASTValidator(parsed)
    } yield validated
  }

  def program: Parser[SapAST] = positioned {
    phrase(block)
  }

  def block: Parser[SapAST] = positioned {
    rep1(statement) ^^ (stmtList => stmtList reduceRight AndThen)
  }

  def statement: Parser[SapAST] = positioned {
    val defineVar = LET() ~ identifier ~ EQUALS() ~ number ~ NEWLINE() ^^ {
      case _ ~ id ~ _ ~ NUMBER(num) ~ _ => DefineVariable(Identifier(id.id, id.pos), num)
    }
    val assignNumber = identifier ~ EQUALS() ~ number ~ NEWLINE() ^^ {
      case id0 ~ _ ~ NUMBER(number) ~ _ => AssignNumber(Identifier(id0.id, id0.pos), number)
    }
    val assignVariable = identifier ~ EQUALS() ~ identifier ~ NEWLINE() ^^ {
      case id0 ~ _ ~ id1 ~ _ => AssignVariable(Identifier(id0.id, id0.pos), Identifier(id1.id, id1.pos))
    }
    val label = LABEL() ~ identifier ~ NEWLINE() ^^ {
      case _ ~ IDENTIFIER(name) ~ _ => Label(name)
    }
    val goto = GOTO() ~ identifier ~ NEWLINE() ^^ {
      case g ~ IDENTIFIER(id) ~ _ => Goto(id, g.pos)
    }
    val addVariables = identifier ~ EQUALS() ~ identifier ~ PLUS() ~ identifier ~ NEWLINE() ^^ {
      case id0 ~ _ ~ id1 ~ _ ~ id2 ~ _ => AssignExpression(Identifier(id0.id, id0.pos), Addition(IdToId(Identifier(id1.id, id1.pos), Identifier(id2.id, id2.pos))))
    }
    val addNumberToVariable = identifier ~ EQUALS() ~ identifier ~ PLUS() ~ number ~ NEWLINE() ^^ {
      case id0 ~ _ ~ id1 ~ _ ~ num ~ _ => AssignExpression(Identifier(id0.id, id0.pos), Addition(IdToNumber(Identifier(id1.id, id1.pos), num.number)))
    }
    val addVariableToNumber = identifier ~ EQUALS() ~ number ~ PLUS() ~ identifier ~ NEWLINE() ^^ {
      case id0 ~ _ ~ num ~ _ ~ id1 ~ _ => AssignExpression(Identifier(id0.id, id0.pos), Addition(NumberToId(num.number, Identifier(id1.id, id1.pos))))
    }
    val addNumbers = identifier ~ EQUALS() ~ number ~ PLUS() ~ number ~ NEWLINE() ^^ {
      case id0 ~ _ ~ num0 ~ _ ~ num1 ~ _ => AssignExpression(Identifier(id0.id, id0.pos), Addition(NumberToNumber(num0.number, num1.number)))
    }
    val minusVariables = identifier ~ EQUALS() ~ identifier ~ MINUS() ~ identifier ~ NEWLINE() ^^ {
      case id0 ~ _ ~ id1 ~ _ ~ id2 ~ _ => AssignExpression(Identifier(id0.id, id0.pos), Subtraction(IdToId(Identifier(id1.id, id1.pos), Identifier(id2.id, id2.pos))))
    }
    val minusNumberToVariable = identifier ~ EQUALS() ~ identifier ~ MINUS() ~ number ~ NEWLINE() ^^ {
      case id0 ~ _ ~ id1 ~ _ ~ num ~ _ => AssignExpression(Identifier(id0.id, id0.pos), Subtraction(IdToNumber(Identifier(id1.id, id1.pos), num.number)))
    }
    val minusVariableToNumber = identifier ~ EQUALS() ~ number ~ MINUS() ~ identifier ~ NEWLINE() ^^ {
      case id0 ~ _ ~ num ~ _ ~ id1 ~ _ => AssignExpression(Identifier(id0.id, id0.pos), Subtraction(NumberToId(num.number, Identifier(id1.id, id1.pos))))
    }
    val minusNumbers = identifier ~ EQUALS() ~ number ~ MINUS() ~ number ~ NEWLINE() ^^ {
      case id0 ~ _ ~ num0 ~ _ ~ num1 ~ _ => AssignExpression(Identifier(id0.id, id0.pos), Subtraction(NumberToNumber(num0.number, num1.number)))
    }
    val output = OUTPUT() ~ identifier ~ NEWLINE() ^^ {
      case _ ~ id ~ _ => Output(Identifier(id.id, id.pos))
    }
    val halt = HALT() ~ NEWLINE() ^^ {
      case _ ~ _ => Halt()
    }
    defineVar | assignNumber| assignVariable | label | goto |
      addNumbers| addNumberToVariable | addVariables| addVariableToNumber |
      minusNumbers| minusNumberToVariable | minusVariables| minusVariableToNumber |
      output | halt
  }


  private def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id@IDENTIFIER(_) => id })
  }

  private def number: Parser[NUMBER] = positioned {
    accept("number", { case id@NUMBER(_) => id })
  }
}
