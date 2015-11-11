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

  private val OneToNineTranslations = OneToNine map (_._2)

  def translate(number: Number): Translation = {
    def concatTranslation(soFar: Translation, next: Translation): Translation = {
      val separator =
        if (soFar.isEmpty) ""
        else if (soFar.endsWith("ty") && OneToNineTranslations.contains(next)) "-"
        else " "

      s"$soFar$separator$next"
    }

    if (number == 0) Zero._2
    else {
      triplets(number)
        .zip(TripletTranslations)
        .reverse
        .collect {
          case (tripletValue, tripletTranslation) if tripletValue % 1000 > 0 =>
            translateLT1000(tripletValue % 1000) + tripletTranslation
        }
        .mkString(" ")
    }
  }

  private def triplets(n: Number) = Stream.iterate(n)(_ / 1000)

  private val TripletTranslations =
    Seq("", " thousand", " million", " billion", " trillion")

  private def translateLT1000(number: Number, soFar: Translation = ""): Translation =
    OneTo999
      .filter (_._1 <= number)
      .headOption map {
        case (100, hundred) =>
          val factor = number / 100
          translateLT1000(number - factor * 100, translateLT1000(factor) + hundred)
        case (n, t) =>
          val sep =
            if (soFar.isEmpty) ""
            else if (soFar.endsWith("ty") && OneToNineTranslations.contains(t)) "-"
            else " "
          translateLT1000(number - n, s"$soFar$sep$t")
      } getOrElse soFar
}

object Lexicon {
  private type LexiconEntry = (Number, Translation)
  private type Lexicon = Seq[LexiconEntry]
  private val LexiconEntry = (_: Number, _: Translation)

  implicit val leOrdering: Ordering[LexiconEntry] =
    Ordering.by[LexiconEntry, Number](_._1).reverse

  private implicit def pairsToLexicon(pairs: Iterable[(Number, Translation)]): Lexicon =
    pairs.toSeq sorted

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

  private val Hundred = LexiconEntry(100, " hundred")

  val OneTo999: Lexicon = Seq(Hundred) ++ OneTo99
}