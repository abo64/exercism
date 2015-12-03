import scala.annotation.tailrec

object PrimeFactors {

  // needed for the tests to compile and run
  def apply() = this

  type Factors = List[Int]

  def primeFactors(number: Long): Factors = {
    unfoldRight((number, 2))(findNext)
  }

  private type Division = (Long, Int)

  @tailrec
  private def findNext(division: Division): Option[(Int,Division)] = {
    val (dividend, divisor) = division
    if (dividend == 1) None
    else if (divisor goesCleanlyInto dividend) Some(divisor, (dividend / divisor, divisor))
    else findNext((dividend, divisor + 1))
  }

  implicit class IntOps(divisor: Int) {
    def goesCleanlyInto(dividend: Long): Boolean =
      dividend % divisor == 0
  }

  private def unfoldRight[A,B](seed: B)(f: B => Option[(A,B)]): List[A] =
    f(seed) match {
    case None => Nil
    case Some((a,b)) => a :: unfoldRight(b)(f)
  }
}
