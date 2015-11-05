import scala.annotation.tailrec

object Octal {
  type Octal = String

  def octalToInt(octal: Octal): Int = {
    def isOctalDigit(digit: Char): Boolean = "01234567" contains digit

    @tailrec def loop(oct: Seq[Char], result: Int): Int = oct match {
      case Nil => result
      case h +: t => loop(t, result * 8 + h.asDigit)
    }

    require(octal.nonEmpty && (octal forall isOctalDigit))
    loop(octal, 0)
  }

  def intToOctal(int: Int): Octal = {
    @tailrec def loop(int: Int, result: Octal): Octal = {
      val newInt = int / 8
      val remainder = int % 8
      val newResult = remainder.toString + result

      if (newInt == 0) newResult
      else loop(newInt, newResult)
    }

    loop(int, "")
  }
}
