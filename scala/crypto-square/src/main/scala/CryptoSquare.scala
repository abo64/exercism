import scala.annotation.tailrec

object CryptoSquare {

  // needed for the tests to compile and run
  def apply() = this

  type PlainText = String
  type NormalizedPlainText = String
  type SquareSize = Int
  type TextSegments = Seq[String]
  type NormalizedCipherText = String
  type CipherText = String

  def normalizePlaintext(text: PlainText): NormalizedPlainText =
    text filter (_.isLetterOrDigit) map (_.toLower)

  def squareSize(text: PlainText): SquareSize = {
    import math._
    val textLength = normalizePlaintext(text).length
    ceil(sqrt(textLength)) toInt
  }

  private val EmptyPlainTextSegments = List("")

  def plaintextSegments(text: PlainText): TextSegments = {
    val normalizedPlainText = normalizePlaintext(text)

    if (normalizedPlainText.isEmpty) EmptyPlainTextSegments
    else normalizedPlainText grouped squareSize(normalizedPlainText) toList
  }

  private def cipherTextSegments(text: PlainText): TextSegments = {
    def addRowChar(col: Int)(cipherText: CipherText, plainTextRow: PlainText): CipherText =
      plainTextRow.lift(col) map (cipherText + _) getOrElse cipherText

    val segments = plaintextSegments(text)
    val squareColumns = (0 until squareSize(text))
    squareColumns map { col =>
      segments.foldLeft("")(addRowChar(col))
    }
  }

  def normalizedCiphertext(text: PlainText): NormalizedCipherText =
    cipherTextSegments(text) mkString(" ")

  def ciphertext(text: PlainText): CipherText =
    cipherTextSegments(text) mkString
}
