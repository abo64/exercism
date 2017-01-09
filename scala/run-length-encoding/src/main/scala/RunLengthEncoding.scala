object RunLengthEncoding {
  type Plain = String
  type Encoded = String

  def encode(str: Plain): Encoded = {
    def encodeGroup(xs: Seq[Char]): Seq[Char] =
      if (xs.length > 1) s"${xs.length}${xs.head}"
      else xs mkString

    splitByEquals(str) flatMap encodeGroup mkString
  }


  def decode(str: Encoded): Plain = {
    def decodeList(xs: List[Char]): List[Char] = 
    if (xs.isEmpty) List()
    else {
      val (digits, y::ys) = xs span (_.isDigit)
      if (digits.isEmpty) y::decodeList(ys)
      else List.fill(digits.mkString toInt)(y) ::: decodeList(ys)
    }

    decodeList(str.toList) mkString
  }

  private def splitByEquals[T](xs: Seq[T]): Seq[Seq[T]] =
    xs match {
      case Seq() => Seq()
      case x +: xss =>
        val fs = xs takeWhile (_ == x)
        fs +: splitByEquals(xs drop fs.length)
    }
}
