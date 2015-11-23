import Say._
import Lexicon._

object Say {
  type Number = Long
  type Translation = String
  type Separator = String

  def inEnglish(number: Number): Option[Translation] =
    if (number == 0) Some("zero")
    else Some(number) filter isWithinBounds map (translate(_, ""))

  private val LowerBound: Number = 1L
  private val UpperBound: Number = 999999999999L

  private def isWithinBounds(number: Number) =
    number >= LowerBound && number <= UpperBound

  // transfered back from my Haskell solution
  private def translate(number: Number, separator: Separator): Translation =
    if (number <= 0) ""
    else {
      val Some((found, foundTranslation)) = lexicon lookupLE number
      val factor =
        if (found >= 100) number / found
        else 0
      val factorTranslation =
        if (factor > 0) translate(factor, "") + " "
        else ""
      val newNumber =
        if (factor > 0) number % found
        else number - found
      val newSeparator =
        if (found < 20) ""
        else if (found >= 20 && found <= 90) "-"
        else " "

      val translationHead = separator + factorTranslation + foundTranslation
      val translationTail = translate(newNumber, newSeparator)
      translationHead + translationTail
    }
}

object Lexicon {
  private type LexiconEntry = (Number, Translation)
  private type Lexicon = Seq[LexiconEntry]

  val lexicon: Lexicon = Seq(
   (1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"),
   (6, "six"), (7, "seven"), (8, "eight"), (9, "nine"), (10, "ten"),
   (11, "eleven"), (12, "twelve"), (13, "thirteen"), (14, "fourteen"),
   (15, "fifteen"), (16, "sixteen"), (17, "seventeen"), (18, "eighteen"),
   (19, "nineteen"), (20, "twenty"), (30, "thirty"), (40, "forty"),
   (50, "fifty"), (60, "sixty"), (70, "seventy"), (80, "eighty"), (90, "ninety"),
   (100, "hundred"), (1000, "thousand"), (1000000, "million"), (1000000000, "billion")) map {
    case (k,v) => (k.toLong,v)
  } reverse

  implicit class LexiconOps(lexicon: Lexicon) {
    def lookupLE(number: Number): Option[LexiconEntry] =
      lexicon find (_._1 <= number)
  }
}