import Luhn._
import scala.annotation.tailrec

class Luhn(number: Number) {

  lazy val checkDigit: Digit =
    addends last

  lazy val addends: Digits = {
    val digitsWithIndex =
      number.toString.map(_.asDigit).reverse.zipWithIndex.reverse
    digitsWithIndex map transformDigit
  }

  lazy val checksum: Checksum =
    addends.sum % 10

  lazy val isValid: Boolean =
    checksum == 0

  lazy val create: Number =
    createLuhn(number)
}

object Luhn {
  type Number = Long
  type Digit = Int
  type Digits = Seq[Digit]
  type Checksum = Int

  def apply(number: Number) = new Luhn(number)

  private def transformDigit(digit: Digit, index: Int): Digit = {
    val isSecondDigit = (index + 1) % 2 == 0
    if (isSecondDigit) {
      val doubledDigit = digit * 2
      val result =
        if (doubledDigit > 9) doubledDigit - 9 else doubledDigit
      result
    } else digit
  }

  private def createLuhn(number: Number): Number = {
    @tailrec def loop(checkDigit: Digit): Number = {
      val candidate: Number = (number.toString + checkDigit).toLong
      if (Luhn(candidate).isValid) candidate
      else loop(checkDigit + 1)
    }

    loop(0)
  }

  implicit def function2AsTupled[A,B,C](f: (A,B) => C): ((A,B)) => C =
    f.tupled
}