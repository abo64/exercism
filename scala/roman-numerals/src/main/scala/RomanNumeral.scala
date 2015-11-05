import RomanNumeral._

class RomanNumeral(number: ArabicNumber) {
  lazy val value: RomanNumber = {
    def decimalFactor(n: Int) = math.pow(10, n) toInt
    val digitsWithIndex: Seq[(Char, Int)] =
      number.toString.reverse.zipWithIndex.reverse
    val digitsWithDecimalFactor: Seq[(Int, Int)] =
      digitsWithIndex map { case (digit, index) =>
        (digit.toString.toInt, decimalFactor(index)) }
    digitsWithDecimalFactor map digitToRoman mkString
  }
}

object RomanNumeral {
  type ArabicNumber = Int
  type RomanNumber = String

  def apply(number: ArabicNumber) = new RomanNumeral(number)

  // not really elegant, but at least it seems to work
  val digitToRoman = {
    val decimalFactorToRoman: Map[Int, (String, String, String)] = Map(
      1 -> ("I", "V", "IX"),
      10 -> ("X", "L", "XC"),
      100 -> ("C", "D", "CM"),
      1000 -> ("M", "ↁ", "Mↂ")
    )

    def asRomanNumber(digit: ArabicNumber, factor: Int): RomanNumber = {
      val (one, five, nine) = decimalFactorToRoman(factor)

      if (digit == 0) ""
      else if (digit <= 3) one * digit
      else if (digit == 4)  one + five
      else if (digit <= 8) five + (one * (digit - 5))
      else nine
    }

    (asRomanNumber _).tupled
  }
}
