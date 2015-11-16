import scala.annotation.tailrec

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
    val segments = plaintextSegments(text)
    val segmentsAsCharSeqs: Seq[Seq[Char]] = segments map (_.toSeq)
    val cipherSegmentsAsCharSeqs: Seq[Seq[Char]] = zipN(segmentsAsCharSeqs:_*)
    val cipherSegments: Seq[String] = cipherSegmentsAsCharSeqs map (_.mkString)
    cipherSegments mkString(" ")
  }

  private def zipN[T](seqs: Seq[T]*): Seq[Seq[T]] = {
    @tailrec
    def loop(seqs: Seq[Seq[T]], acc: Seq[Seq[T]]): Seq[Seq[T]] =
      if (seqs.isEmpty) acc
      else {
        val heads = seqs flatMap (_.take(1))
        val tails = seqs map (_.drop(1)) filterNot (_.isEmpty)
        loop(tails, acc :+ heads)
      }

    loop(seqs, Seq())
  }

  def ciphertext(text: PlainText): CipherText =
    normalizedCiphertext(text) filterNot (_.isSpaceChar)
}
