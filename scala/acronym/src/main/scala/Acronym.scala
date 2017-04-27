object Acronym {

  def abbreviate(phrase: String): String = {
    val acronymChar: PartialFunction[(Char, Char), Char] = {
      case ((x, y)) if (!x.isLetter && y.isLetter) => y.toUpper
      case ((x, y)) if (!x.isUpper && y.isUpper) => y
    }

    val charPairs = ' ' +: phrase zip phrase

    charPairs collect acronymChar mkString
  }
}
