object Acronym {

  def abbreviate(phrase: String): String = {
    def acronymChar(x: Char, y: Char): Option[Char] =
      if (!x.isLetter && y.isLetter) Some(y.toUpper)
      else if (!x.isUpper && y.isUpper) Some(y)
      else None

    val charPairs = ' ' +: phrase zip phrase

    charPairs flatMap (acronymChar _).tupled.andThen(_.toList) mkString
  }
}
