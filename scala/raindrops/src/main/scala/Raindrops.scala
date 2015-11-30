import scala.collection.SortedMap

object Raindrops {

  // needed for the tests to compile and run
  def apply() = this

  private val Raindrops =
    SortedMap(3 -> "Pling", 5 -> "Plang", 7 -> "Plong")

  def convert(number: Int): String = {
    def hasPrimeFactor(primeFactor: Int): Boolean =
      number % primeFactor == 0

    val result =
      Raindrops.foldLeft("") {
        case (result, (n, str)) =>
          if (hasPrimeFactor(n)) result + str
          else result
      }
    if (result.nonEmpty) result else number.toString
  }
}
