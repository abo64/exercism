object CryptoSquare {

  // needed for the tests to compile and run
  def apply() = this

  type PlainText = String
  type NormalizedPlainText = String
  type SquareSize = Int
  type PlainTextSegments = List[String]
  type NormalizedCipherText = String
  type CipherText = String

  def normalizePlaintext(text: PlainText): NormalizedPlainText =
    text filter (_.isLetterOrDigit) map (_.toLower)

  def squareSize(text: PlainText): SquareSize = {
    import math._
    val textLength = text.length
    ceil(sqrt(textLength)) toInt
  }

  private val EmptyPlainTextSegments = List("")

  def plaintextSegments(text: PlainText): PlainTextSegments = {
    val normalizedPlainText = normalizePlaintext(text)

    if (normalizedPlainText.isEmpty) EmptyPlainTextSegments
    else normalizedPlainText grouped squareSize(normalizedPlainText) toList
  }

  def normalizedCiphertext(text: PlainText): NormalizedCipherText = {
    def charAt(charPos: Int): PartialFunction[String, Char] =
      { case str if str.length > charPos => str.charAt(charPos) }
    def charsAtSamePosToString(segments: PlainTextSegments)(charPos: Int): String =
      segments collect charAt(charPos) mkString

    val segments = plaintextSegments(text)
    val segmentMaxLength = segments map (_.length) max
    val segmentCharPositions = (0 to segmentMaxLength)
    val cypherSegments: Seq[String] =
      segmentCharPositions map charsAtSamePosToString(segments)
    cypherSegments filter (_.nonEmpty) mkString (" ")

    // the first idea is not always best
//    val indexedChars: List[(Char, Int)] =
//      plaintextSegments(text) flatMap (_.zipWithIndex)
//    val charsGroupedByPosition = SortedMap(indexedChars groupBy (_._2) toSeq: _*)
//    val charsByPosition: Seq[List[(Char, Int)]] = charsGroupedByPosition.values toSeq
//    val stringsAsCharSeq: Seq[List[Char]] = charsByPosition.map(_.map(_._1))
//    stringsAsCharSeq map (_.mkString) mkString (" ")
  }

  def ciphertext(text: PlainText): CipherText =
    normalizedCiphertext(text) filter (_.isLetterOrDigit)
}
