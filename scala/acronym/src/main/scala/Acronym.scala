import Acronym._

class Acronym(phrase: String) {

  def abbreviate: Abbreviation = {
    val sanitizedPhrase = phrase filterNot noise
    val words = splitWords(sanitizedPhrase)
    val upperFirstWordChars = words map (_.head.toUpper)
    upperFirstWordChars mkString
  }
}

object Acronym {
  type Phrase = String
  type Word = String
  type Abbreviation = String

  def apply(phrase: Phrase): Acronym = new Acronym(phrase)

  private val WordSeparators = " -"

  private def noise(char: Char): Boolean =
    !char.isLetter && !WordSeparators.contains(char)

  private def splitWords(phrase: Phrase): Seq[Word] = {
    def splitCamelCaseWords(word: Word): Seq[Word] = {
      // stolen from
      // http://stackoverflow.com/questions/7593969/regex-to-split-camelcase-or-titlecase-advanced
      word split "(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])"
    }

    val words = phrase split s"[$WordSeparators]"
    words flatMap splitCamelCaseWords
  }
}
