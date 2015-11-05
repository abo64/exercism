import Say._
import Lexicon._

object Say {
  type Number = Long
  type Translation = String

  def inEnglish(number: Number): Option[Translation] =
    Some(number) filter isWithinBounds map translate

  private val LowerBound: Number = 0L
  private val UpperBound: Number = 999999999999L

  private def isWithinBounds(number: Number) =
    number >= LowerBound && number <= UpperBound

  def translate(number: Number): Translation = {
    val start = ReducibleTranslation(number, "")
    val fullTranslation = NumberLexicon.foldLeft(start)(_ reduce _)
    val ReducibleTranslation(0, translation) = fullTranslation
    translation
  }
}

case class ReducibleTranslation(number: Number, translation: Translation)
{
  def reduce(other: ReducibleTranslation): ReducibleTranslation = {
    if (number < other.number) this
    else if (number >= 100) this / other
    else if (other != Zero || translation == "") this - other
    else this
  }

  def -(other: ReducibleTranslation): ReducibleTranslation = {
    val nextNumber = number - other.number
    val separator =
      if (translation.isEmpty) ""
      else if (translation.endsWith("ty")) "-"
      else " "
    val nextTranslation = s"$translation${separator}${other.translation}"
    ReducibleTranslation(nextNumber, nextTranslation)
  }

  def /(other: ReducibleTranslation): ReducibleTranslation = {
    val division = number / other.number
    val oneTo999 = translate(division)
    this - ReducibleTranslation(division * other.number, s"$oneTo999 ${other.translation}")
  }
}

object Lexicon {
  private type LexiconEntry = ReducibleTranslation
  private type Lexicon = Seq[LexiconEntry]
  private val LexiconEntry = ReducibleTranslation

  implicit def pairToLexiconEntry(pair: (Number,Translation)): LexiconEntry =
    LexiconEntry(pair._1, pair._2)

  implicit val leOrdering: Ordering[LexiconEntry] =
    Ordering.by[LexiconEntry,Number](_.number).reverse

  private implicit def pairsToLexicon(pairs: Iterable[(Number,Translation)]): Lexicon =
    pairs.toSeq map pairToLexiconEntry sorted

  val Zero = LexiconEntry(0, "zero")

  val OneToNine: Lexicon =
    Set(1L -> "one", 2L -> "two", 3L -> "three", 4L -> "four",
        5L -> "five", 6L -> "six", 7L -> "seven", 8L -> "eight",
        9L -> "nine")

  val TenToNineteen: Lexicon =
    Set(10L -> "ten", 11L -> "eleven", 12L -> "twelve", 13L -> "thirteen",
        14L -> "fourteen", 15L -> "fifteen", 16L -> "sixteen", 17L -> "seventeen",
        18L -> "eighteen", 19L -> "nineteen")

  val TwentyTo99: Lexicon =
    Set(20L -> "twenty", 30L -> "thirty", 40L -> "forty",
      50L -> "fifty", 60L -> "sixty", 70L -> "seventy", 80L -> "eighty", 90L -> "ninety")

  val OneTo99: Lexicon = TwentyTo99 ++ TenToNineteen ++ OneToNine

  private val Hundreds = LexiconEntry(100, "hundred")

  val OneTo999: Lexicon = Seq(Hundreds) ++ OneTo99

  private val Thousands = LexiconEntry(1000, "thousand")

  private val Millions = LexiconEntry(1000000, "million")

  private val Billions = LexiconEntry(1000000000, "billion")

  val NumberLexicon: Lexicon =
    Seq(Billions, Millions, Thousands, Hundreds) ++ OneTo99 :+ Zero
}