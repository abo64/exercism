import scala.annotation.tailrec

object Trinary {
  type Trinary = String
  def trinaryToInt(trinary: Trinary): Int = {
    def isTrinaryDigit(digit: Char): Boolean = "012" contains digit

    @tailrec def loop(oct: Seq[Char], result: Int): Int = oct match {
      case Nil => result
      case h +: t => loop(t, result * 3 + h.asDigit)
    }

    require(trinary.nonEmpty && (trinary forall isTrinaryDigit))
    loop(trinary, 0)

  }

  def intToTrinary(int: Int): Trinary = {
    @tailrec def loop(int: Int, result: Trinary): Trinary = {
      val newInt = int / 3
      val remainder = int % 3
      val newResult = remainder.toString + result

      if (newInt == 0) newResult
      else loop(newInt, newResult)
    }

    loop(int, "")
  }
}
