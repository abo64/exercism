import collection.mutable
import util.Random
import Robot._

class Robot {

  private var myName: String = newRandomName

  def name: String = myName

  def reset(): Unit =
    myName = newRandomName
}

object Robot {
  private[this] val random = new Random

  private[this] val Letters: Seq[Char] = ('A' to 'Z')
  private[this] val Digits: Seq[Char] = ('0' to '9')

  private[this] def oneOf[T](ts: Seq[T]): T = ts(random.nextInt(ts.size))
  private[this] def randomLetter: Char = oneOf(Letters)
  private[this] def randomDigit: Char = oneOf(Digits)
  private[this] def randomString(length: Int, char: => Char): String =
    Seq.fill(length)(char) mkString

  // just for fun: a poor man's DSL
  private[this] implicit class IntOps(size: Int) {
    def letters = randomString(size, randomLetter)
    def digits = randomString(size, randomDigit)
  }
  private[this] implicit class StringOps(str: String) {
    def and(other: String): String = str + other
    def or(other: String): String = if (random.nextBoolean) str else other
  }
  private[this] def tryAgain[T](t: => T): T = t
  private[this] val alreadyExists = mutable.Set[String]() // not production-ready: no growth limit
  private[this] def take(name: String): String = {
    alreadyExists += name
    name
  }

  def newRandomName: String = {
    val candidate = (2 letters) and (3 digits)

    if (alreadyExists(candidate)) tryAgain(newRandomName)
    else take(candidate)
  }
}