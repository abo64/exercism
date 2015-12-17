import Cipher._
import scala.util.Random

class Cipher(val key: Key) {
  require(key.nonEmpty, "Invalid key - must be nonEmpty")
  require(key forall (_.isLower), s"Invalid key - contains caps or numerics: $key")

  def encode(clearText: ClearText): CipherText =
    shiftText(clearText, Right)

  def decode(cipherText: CipherText): ClearText =
    shiftText(cipherText, Left)

  private val keyOffsets = {
    def distanceFromA(char: Char): Int = char.toInt - 'a'.toInt

    val cyclicKeyChars = Stream.continually(key).flatten
    cyclicKeyChars map distanceFromA
  }

  private def shiftText(text: String, direction: Int => Int): String = {
    val shiftChar: (Char,Int) => Char = {
      case (char, offset) =>
        val amount = char.toInt - LowerBound + direction(offset)
        val cyclicShifted =
          if (amount < 0) UpperBound + amount + 1
          else if (amount > MaxOffset) LowerBound + amount - MaxOffset - 1
          else LowerBound + amount
        cyclicShifted.toChar
    }

    text zip keyOffsets map shiftChar mkString
  }
}

object Cipher {
  type Key = String
  type ClearText = String
  type CipherText = String

  def apply(key: Option[Key]) = {
    new Cipher(key getOrElse randomKey)
  }

  private val chars = 'a' to 'z'
  val MaxOffset = chars.length - 1
  val LowerBound = 'a'.toInt
  val UpperBound = 'z'.toInt
  val Left: Int => Int = (- _)
  val Right: Int => Int = identity

  private def randomKey: Key = {
    val size = 100 + Random.nextInt(100)
    lowerCaseAlpha take(size) mkString
  }

  private def lowerCaseAlpha: Stream[Char] = {
    def nextLowerCaseAlpha: Char = {
      chars charAt (Random.nextInt(chars.length))
    }

    Stream continually nextLowerCaseAlpha
  }

  implicit def function2AsTupled[A,B,C](f: (A,B) => C): ((A,B)) => C =
    f.tupled
}
