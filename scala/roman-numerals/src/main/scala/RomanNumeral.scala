import scala.annotation.tailrec
import RomanNumeral._

class RomanNumeral(arabicNumber: ArabicNumber) {

  lazy val value: RomanNumber =
    toRoman(arabicNumber)
}

object RomanNumeral {
  private type ArabicNumber = Int
  private type RomanNumber = String

  def apply(number: ArabicNumber) = new RomanNumeral(number)

  @tailrec
  private def toRoman(number: ArabicNumber, result: RomanNumber = ""): RomanNumber =
    ArabicToRoman.find(_._1 <= number) match {
      case None => result
      case Some((digitValue, romanDigit)) =>
        toRoman(number - digitValue, result + romanDigit)
    }

  private[this] val ArabicToRoman = Seq(
    1000 -> "M",
    900  -> "CM",
    500  -> "D",
    400  -> "CD",
    100  -> "C",
    90   -> "XC",
    50   -> "L",
    40   -> "XL",
    10   -> "X",
    9    -> "IX",
    5    -> "V",
    4    -> "IV",
    1    -> "I"
  )
}
