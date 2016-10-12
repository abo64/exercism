import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.traverse._

object Raindrops {

  // needed for the tests to compile and run
  def apply() = this

  private val factorMessages =
    List((3, "Pling"),
         (5, "Plang"),
         (7, "Plong"))

  def convert(number: Int): String = {
    def hasFactor(factor: Int): Boolean =
      number % factor == 0

    val getMessage: ((Int, String)) => Option[String] = {
      case (factor, message) =>
        Option(factor) filter hasFactor map const(message)
    }
    // Haskell: getMessage (f, xs) = guard (x `mod` f == 0) >> return xs

    factorMessages foldMap getMessage getOrElse number.toString
    // Haskell: fromMaybe (show x) (foldMap getMessage factorMessages)
  }

  private def const[A,B](a: A)(ignore: B): A = a
}
