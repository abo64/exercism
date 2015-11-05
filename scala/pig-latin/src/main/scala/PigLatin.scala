object PigLatin {
  type ClearText = String
  type PigLatin = String

  def translate(clearText: ClearText): PigLatin = {
    val words = clearText split(" ")
    words map wordToPigLatin mkString " "
  }

  private val PigEnding = "ay"

  private val Vowel = "aeiou"
  private val Consonant = s"(qu|squ|[^$Vowel]+)"
  private val ConsonantWord = s"$Consonant(.*)".r

  private def wordToPigLatin(word: ClearText): PigLatin = word match {
    case ConsonantWord(consonant,rest) => s"$rest$consonant$PigEnding"
    case vowelWord => s"$vowelWord$PigEnding"
  }
}
