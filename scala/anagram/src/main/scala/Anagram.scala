class Anagram(word: String) {

  lazy val lowerCaseWord = word toLowerCase
  lazy val wordChars = chars(word)

  type StringPredicate = String => Boolean

  lazy val sameChars: StringPredicate = chars(_) == wordChars
  lazy val isIdentical: StringPredicate = _.toLowerCase == lowerCaseWord
  lazy val sameCharsButNotIdentical: StringPredicate =
    candidate => sameChars(candidate) && !isIdentical(candidate)

  def matches(candidates: Seq[String]): Seq[String] =
    candidates filter sameCharsButNotIdentical

  private def chars(word: String): Seq[Char] = word.toLowerCase sorted
}
