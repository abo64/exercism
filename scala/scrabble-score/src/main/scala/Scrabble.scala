import Scrabble._

class Scrabble {
  def scoreLetter(letter: Char): Int =
    letterValue(letter.toUpper)

  def scoreWord(word: String): Int =
    word map scoreLetter sum
}

object Scrabble {
  val letterValue = {
    def valueMap(letters: Char*)(value: Int): Map[Char, Int] =
      letters map ((_, value)) toMap

    valueMap('A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T')(1) ++
    valueMap('D', 'G')(2) ++ valueMap('B', 'C', 'M', 'P')(3) ++
    valueMap('F', 'H', 'V', 'W', 'Y')(4) ++ valueMap('K')(5) ++
    valueMap('J', 'X')(8) ++ valueMap('Q', 'Z')(10)
  }
}
