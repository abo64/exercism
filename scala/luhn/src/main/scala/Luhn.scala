import Luhn._

class Luhn(number: Number) {

  lazy val checkDigit: Digit =
    addends last

  lazy val addends: Digits = {
    val digits = number.toString
    val digitsWithIndex =
      digits.map(_.asDigit).zipWithIndex
    digitsWithIndex map luhnTransform(digits.length - 1)
  }

  lazy val checksum: Checksum =
    addends.sum % 10

  lazy val isValid: Boolean =
    checksum == 0

  lazy val create: Number = {
    val isValidLuhn = Luhn(_:Number).isValid
    val candidates = (0 to 9) map (number*10 + _)
    candidates filter isValidLuhn head
  }
}

object Luhn {
  type Number = Long
  type Digit = Int
  type Digits = Seq[Digit]
  type Checksum = Int

  def apply(number: Number) = new Luhn(number)

  private def luhnTransform(maxIndex: Int)(digit: Digit, index: Int): Digit = {
    def odd(number: Int) = number % 2 != 0
    def luhnTransformedDigit: Digit = {
      val doubledDigit = digit * 2
      if (doubledDigit > 9) doubledDigit - 9 else doubledDigit
    }

    if (odd(maxIndex - index)) luhnTransformedDigit else digit
  }

  implicit def function2AsTupled[A,B,C](f: (A,B) => C): ((A,B)) => C =
    f.tupled
}