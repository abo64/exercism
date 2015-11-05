object FoodChain {
  def song: String =
    verses(Lexicon.size)

  private val VerseSeparator = "\n\n"

  type Verse = String

  private def verses(howMany: Int): String = {
    val verses: Seq[Verse] =
      ((0 until howMany) map verse) :+ lastVerse
    verses mkString("", VerseSeparator, VerseSeparator)
  }

  private def verse(n: Int): String = {
    def optionalLine(line: String) =
      if (line.nonEmpty) s"$line\n" else ""

    Intro + animal(n) + ".\n" +
    optionalLine(exclamation(n)) +
    swallow(n) +
    Outro
  }

  private def swallow(n: Int): String =
    if (n > 0) {
      s"She swallowed the ${animal(n)} to catch the ${animal(n - 1)}${description(n - 1)}.\n" +
      swallow(n - 1)
    } else ""

  private val Intro = "I know an old lady who swallowed a "
  private val Outro = "I don't know why she swallowed the fly. Perhaps she'll die."
  val lastVerse = Intro + "horse.\nShe's dead, of course!"

  type Animal = String
  type Exclamation = String
  type Description = String

  private def animal(whichOne: Int): Animal = Lexicon(whichOne)._1
  private def exclamation(whichOne: Int): Exclamation = Lexicon(whichOne)._2
  private def description(whichOne: Int): Exclamation = Lexicon(whichOne)._3

  private val Lexicon: Seq[(Animal,Exclamation,Description)] = Seq(
    ("fly", "", ""),
    ("spider", "It wriggled and jiggled and tickled inside her.", " that wriggled and jiggled and tickled inside her"),
    ("bird", "How absurd to swallow a bird!", ""),
    ("cat", "Imagine that, to swallow a cat!", ""),
    ("dog", "What a hog, to swallow a dog!", ""),
    ("goat", "Just opened her throat and swallowed a goat!", ""),
    ("cow", "I don't know how she swallowed a cow!", "")
  )
}
