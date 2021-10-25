import uk.co.rc418.sap.compiler.compiler.Compiler
import uk.co.rc418.sap.compiler.asm.AsmCompiler.asmToString

object Main {
  def main(args: Array[String]): Unit = {
    val result = Compiler(
      """LET a = 13
        |LET b = 42
        |LABEL dest
        |a = a - b
        |OUTPUT a
        |GOTO dest
        |HALT""".stripMargin)
    result match {
      case Left(value) => println(s"${value.msg} at ${value.location} \n${value.location.longString}")
      case Right(value) => printValue(value)
    }
  }

  private def printValue(value: String): Unit = println(value)
}
