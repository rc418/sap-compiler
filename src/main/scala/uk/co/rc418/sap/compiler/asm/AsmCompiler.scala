package uk.co.rc418.sap.compiler.asm

import uk.co.rc418.sap.compiler.parser
import uk.co.rc418.sap.compiler.parser.{Addition, AndThen, AssignExpression, DefineVariable, Goto, IdToId, Label, SapAST, Subtraction}

import scala.language.implicitConversions


object AsmCompiler {

  def apply(ast: SapAST): Seq[AsmToken] = {
    val ordered = moveAssignmentsToEnd(astToTokenList(ast))
    val raw = convertToAssembly(ordered)
    optimise(raw)
  }

  private def convertToAssembly(astList: Seq[SapAST], label:Option[String] = None): Seq[AsmToken] = {
    if(astList.isEmpty) {
      Seq()
    } else {
      val hd :: tail = astList
      hd match {
        case DefineVariable(id, num) => Seq(LoadMemory(id.id, num)) ++ convertToAssembly(tail)
        case AssignExpression(id, expression) => expression match {
          case Addition(parameters) => parameters match {
            case IdToId(id1, id2) => Seq(LoadA(id1.id, label), Add(id2.id), StoreA(id.id)) ++ convertToAssembly(tail)
          }
          case Subtraction(parameters) => parameters match {
            case IdToId(id1, id2) => Seq(LoadA(id1.id, label), Sub(id2.id), StoreA(id.id)) ++ convertToAssembly(tail)
          }
        }
        case Label(name) => convertToAssembly(tail, Some(name))
        case Goto(dest) => Seq(Jump(dest, label)) ++ convertToAssembly(tail)
        case parser.Output(id) => Seq(LoadA(id.id, label), Output()) ++ convertToAssembly(tail)
        case parser.Halt() => Seq(Halt(label)) ++ convertToAssembly(tail)
      }
    }
  }

  private def moveAssignmentsToEnd(asSeq: Seq[SapAST]): Seq[SapAST] = {
    val assignments = asSeq.filter(x => x.isInstanceOf[DefineVariable])
    val nonAssignments = asSeq.filterNot(x => x.isInstanceOf[DefineVariable])
    nonAssignments ++ assignments
  }

  private def astToTokenList(sapAST: SapAST): Seq[SapAST] = {
    sapAST match {
      case AndThen(step1, step2) => Seq(step1) ++ astToTokenList(step2)
      case _ => Seq(sapAST)
    }
  }

  implicit def asmToString(asmList: Seq[AsmToken]): String = {
    val zippedList: Seq[(AsmToken, Int)] = asmList.zipWithIndex
    val variableAddresses: Map[String, Int] = mapVariables(zippedList)
    val labelAddresses: Map[String, Int] = mapLabels(zippedList)
    asmToString(zippedList, variableAddresses, labelAddresses)
  }

  private def asmToString(zippedList: Seq[(AsmToken, Int)], variableAddresses: Map[String, Int], labelAddresses: Map[String, Int]): String = {
    if(zippedList.isEmpty) {
      ""
    } else {
      val hd :: tail = zippedList
      val current: String = {
        toBinary(hd._2 + 1, 4) + " " + {
          hd match {
            case (t: LoadA, _) => t.asString(toBinary(variableAddresses(t.variable), 8))
            case (t: StoreA, _) => t.asString(toBinary(variableAddresses(t.variable), 8))
            case (t: Add, _) => t.asString(toBinary(variableAddresses(t.variable), 8))
            case (t: Sub, _) => t.asString(toBinary(variableAddresses(t.variable), 8))
            case (t: JumpOnCarry, _) => t.asString(toBinary(labelAddresses(t.destination), 8))
            case (t: Jump, _) => t.asString(toBinary(labelAddresses(t.destination), 8))
            case (t: JumpOnZero, _) => t.asString(toBinary(labelAddresses(t.destination), 8))
            case _ => hd._1.asString("")
          }
        }
      }
      current + "\n" + asmToString(tail, variableAddresses, labelAddresses)
    }
  }

  private def toBinary(value: Int, size: Int): String = {
    "00000000" + value.toBinaryString takeRight size
  }

  private def mapVariables(zippedList: Seq[(AsmToken, Int)]): Map[String, Int] = {
    if(zippedList.isEmpty) {
      Map()
    } else {
      val hd :: tail = zippedList
      hd match {
        case (token: LoadMemory, index: Int) => Map (token.variable -> (index + 1)) ++ mapVariables(tail)
        case _ => mapVariables(tail)
      }
    }
  }


  private def mapLabels(zippedList: Seq[(AsmToken, Int)]): Map[String, Int] = {
    if (zippedList.isEmpty) {
      Map()
    } else {
      val hd :: tail = zippedList
      hd match {
        case (token: AsmToken, index: Int) if token.label.isDefined => Map(token.label.get -> (index + 1)) ++ mapVariables(tail)
        case _ => mapVariables(tail)
      }
    }
  }

  private def dontRemove(a: AsmToken, b: AsmToken) = {
    !(a.isInstanceOf[StoreA] && b.isInstanceOf[LoadA] && a.asInstanceOf[StoreA].variable == b.asInstanceOf[LoadA].variable)
  }

  // Optimisation is to remove LoadA actions that immediately follow a StoreA for the same variable as A will already have the value in it
  private def optimise(raw: Seq[AsmToken]): Seq[AsmToken] =
    raw.head :: raw.sliding(2).collect {
      case Seq(a, b) if dontRemove(a, b) => b
    }.toList
}
