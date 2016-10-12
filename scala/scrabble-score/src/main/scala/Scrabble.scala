//import scalaz.std.list._
//import scalaz.std.math.bigInt._
//import scalaz.syntax.traverse._
import cats.instances.list._
import cats.instances.bigInt._
import cats.syntax.foldable._

import Scrabble._

class Scrabble {
  val scoreLetter: Char => Int =
    letterValue compose (_.toUpper)

  def scoreWord(word: String): Int =
    word.toList foldMap ((BigInt.int2bigInt _) compose scoreLetter) intValue
    // Haskell: getSum . foldMap (Sum . scoreLetter)
}

object Scrabble {
  val letterValue: Map[Char, Int] = {
    def valueMap(letters: Char*)(value: Int): Map[Char, Int] =
      letters map ((_, value)) toMap

    valueMap('A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T')(1) ++
    valueMap('D', 'G')(2) ++ valueMap('B', 'C', 'M', 'P')(3) ++
    valueMap('F', 'H', 'V', 'W', 'Y')(4) ++ valueMap('K')(5) ++
    valueMap('J', 'X')(8) ++ valueMap('Q', 'Z')(10)
  }
}
