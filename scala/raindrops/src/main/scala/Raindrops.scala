import scala.collection.SortedMap

object Raindrops {

  // needed for the tests to compile and run
  def apply() = this

  def convert(number: Int): String = {
    def hasPrimeFactor(primeFactor: Int): Boolean =
      number % primeFactor == 0

    def raindrop(primeFactor: Int, sound: String): String => String =
      str => if (hasPrimeFactor(primeFactor)) str + sound else str

    "" |> raindrop(3, "Pling") |> raindrop(5, "Plang") |> raindrop(7, "Plong") |>
      { result =>
          if (result.nonEmpty) result
          else number.toString
      }
  }

  implicit class Pipe[A](a: A) {
    def |>[B](f: A => B) = f(a)
  }
}
