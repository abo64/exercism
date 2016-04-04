object Pangrams {
  def isPangram(sentence: String): Boolean = {
    val lowerCaseSentence = sentence.toLowerCase
    def isContainedInSentence(char: Char) = lowerCaseSentence.contains(char)

    Alphabet forall isContainedInSentence
  }

  private val Alphabet = 'a' to 'z'
}
