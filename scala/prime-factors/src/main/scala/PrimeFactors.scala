import scala.annotation.tailrec

object PrimeFactors {

  // needed for the tests to compile and run
  def apply() = this

  type Factors = List[Int]

  def primeFactors(number: Long): Factors = {
    @tailrec def loop(dividend: Long, divisor: Int, factors: Factors): Factors = {
      if (dividend == 1) factors.reverse
      else if (divisor goesCleanlyInto dividend) loop(dividend / divisor, divisor, divisor :: factors)
      else loop(dividend, divisor + 1, factors)
    }

    loop(number, 2, List())
  }

  implicit class IntOps(divisor: Int) {
    def goesCleanlyInto(dividend: Long): Boolean =
      dividend % divisor == 0
  }
}
