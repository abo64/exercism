import scala.annotation.tailrec

object SecretHandshake {
  type Binary = String
  type Commands = Seq[String]

  def handshake(int: Int): Commands =
    handshake(intToBinary(int))

  def handshake(binary: Binary): Commands =
    Option(binary) filter validCommands map toCommands getOrElse InvalidCommands

  private def toCommands(binaryCommands: Binary): Commands = {
    def processReverse(commands: Commands): Commands =
      if (commands contains ReverseCommand) {
        commands filterNot (_ == ReverseCommand) reverse
      } else commands

    val commandIndexes =
      binaryCommands.reverse.zipWithIndex filter (_._1 == '1') map (_._2)
    val commands = commandIndexes map AllCommands
    processReverse(commands)
  }

  private def validCommands(binary: Binary): Boolean =
    binary.length <= 5 && (binary forall (b => b == '0' || b == '1'))

  private val InvalidCommands: Commands = Seq()
  private val ReverseCommand = "reverse"
  private val AllCommands: Commands =
    Seq("wink", "double blink", "close your eyes", "jump", ReverseCommand)

  private def intToBinary(int: Int): Binary = {
    @tailrec def loop(int: Int, result: Binary): Binary = {
      val newInt = int / 2
      val remainder = int % 2
      val newResult = remainder.toString + result

      if (newInt == 0) newResult
      else loop(newInt, newResult)
    }

    loop(int, "")
  }
}
