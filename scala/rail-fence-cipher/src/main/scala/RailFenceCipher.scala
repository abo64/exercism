object RailFenceCipher {

  type Plain = String
  type Cipher = String

  def encode(text: Plain, howManyRails: Int): Cipher =
    cipher(text.length, howManyRails) map {
      i => text(i._1)
    } mkString

  def decode(cipherText: Cipher, howManyRails: Int): Plain =
    cipher(cipherText.length, howManyRails) sortBy (_._1) map {
      i => cipherText(i._2)
    } mkString

  // mapping from original to encoded String index
  private def cipher(textLength: Int, howManyRails: Int): Seq[(Int, Int)] = {
    val rails = Iterator.iterate((0, +1)) {
      _ match {
        case (x, inc) if x + inc > 0 && x + inc < howManyRails - 1 => (x + inc, inc)
        case (x, inc) => (x + inc, -inc)
      }
    } map (_._1) toSeq

    val railIndices = rails zip (0 until textLength)

    railIndices.sortBy (_._1).zipWithIndex.map {
      case (((rail, index), newIndex)) => (index, newIndex)
    }
  }
}
