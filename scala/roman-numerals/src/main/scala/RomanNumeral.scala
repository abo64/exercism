import RomanNumeral._

class RomanNumeral(arabicNumber: ArabicNumber) {

  lazy val value: RomanNumber =
    toRoman(arabicNumber)
}

object RomanNumeral {
  private type ArabicNumber = Int
  private type RomanNumber = String

  def apply(number: ArabicNumber) = new RomanNumeral(number)

  private[this] def findNext(number: ArabicNumber) =
    ArabicToRoman.find(_._1 <= number) match {
      case None => None
      case Some((numberValue, romanNumber)) => Some((romanNumber, number - numberValue))
    }

  private def toRoman(number: ArabicNumber): RomanNumber =
    unfoldRight(number)(findNext) mkString

  private[this] def unfoldRight[A,B](seed: B)(f: B => Option[(A,B)]): Seq[A] =
    f(seed) match {
    case None => Seq()
    case Some((a,b)) => a +: unfoldRight(b)(f)
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
