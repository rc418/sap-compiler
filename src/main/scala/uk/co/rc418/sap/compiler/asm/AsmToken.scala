package uk.co.rc418.sap.compiler.asm

sealed trait AsmToken {
  val opCode: String
  val machineCode: String
  val label: Option[String]
  def asString(address: String): String
}

case class LoadA(variable: String, label: Option[String] = None) extends AsmToken {
  override val opCode: String = "LDA"
  override val machineCode: String = "0001"
  override def asString(address: String): String = s"$opCode $address"
}
case class LoadAImmediate(value: Int, label: Option[String] = None) extends AsmToken {
  require(value < 16)
  override val opCode: String = "LDI"
  override val machineCode: String = "0101"
  override def asString(address: String): String = s"$opCode $value"
}
case class StoreA(variable: String, label: Option[String] = None) extends AsmToken {
  override val opCode: String = "STA"
  override val machineCode: String = "0100"
  override def asString(address: String): String = s"$opCode $address"
}
case class Add(variable: String, label: Option[String] = None) extends AsmToken {
  override val opCode: String = "ADD"
  override val machineCode: String = "0010"
  override def asString(address: String): String = s"$opCode $address"
}
case class Sub(variable: String, label: Option[String] = None) extends AsmToken {
  override val opCode: String = "SUB"
  override val machineCode: String = "0011"
  override def asString(address: String): String = s"$opCode $address"
}
case class Output(label: Option[String] = None) extends AsmToken {
  override val opCode: String = "OUT"
  override val machineCode: String = "1110"
  override def asString(address: String): String = s"$opCode"
}
case class JumpOnCarry(destination: String, label: Option[String] = None) extends AsmToken {
  override val opCode: String = "JC"
  override val machineCode: String = "0111"
  override def asString(address: String): String = s"$opCode $address"
}
case class Jump(destination: String, label: Option[String] = None) extends AsmToken {
  override val opCode: String = "JMP"
  override val machineCode: String = "0110"
  override def asString(address: String): String = s"$opCode $address"
}
case class JumpOnZero(destination: String, label: Option[String] = None) extends AsmToken {
  override val opCode: String = "JZ"
  override val machineCode: String = "1000"
  override def asString(address: String): String = s"$opCode $address"
}
case class Halt(label: Option[String] = None) extends AsmToken {
  override val opCode: String = "HLT"
  override val machineCode: String = "1111"
  override def asString(address: String): String = s"$opCode"
}
case class LoadMemory(variable: String, value: Int, label: Option[String] = None) extends AsmToken {
  override val opCode: String = "MOV"
  override val machineCode: String = "N/A"
  override def asString(address: String): String = {
    val binaryString = "00000000" + value.toBinaryString takeRight 8
    s"$opCode $binaryString"
  }
}