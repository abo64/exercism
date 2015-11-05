import Cipher._
import scala.util.Random

class Cipher(val key: Key) {
  require(key.nonEmpty, "Invalid key - must be nonEmpty")
  require(key forall (_.isLower), s"Invalid key - contains caps or numerics: $key")

  def encode(clearText: ClearText): CipherText =
    clearText.zipWithIndex map shift(Right) mkString

  def decode(cipherText: CipherText): ClearText =
    cipherText.zipWithIndex map shift(Left) mkString

  private def shift(right: Boolean)(char: Char, pos: Int): Char = {
    val sign: Int => Int = if (right) (+ _) else (- _)
    val offset = (key(pos % key.size) - ZeroOffset)
    val int = ZeroOffset + ((char.toInt + sign(offset) - ZeroOffset) % MaxShift)
    int.toChar
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
  val ZeroOffset = 'a'.toInt
  val MaxShift = chars.length
  val Left = false
  val Right = true

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
