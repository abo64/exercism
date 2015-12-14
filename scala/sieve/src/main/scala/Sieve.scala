import scala.collection.mutable.ListBuffer

object Sieve {
  type Prime = Int

  def primesUpTo(n: Int): Seq[Prime] =
    (2 to n).foldLeft(ListBuffer[Int]())(addIfPrime) toSeq

  private def addIfPrime(primes: ListBuffer[Int], n: Int): ListBuffer[Int] = {
    def squareLE(i: Int) = i*i <= n
    def notDivisibleBy(i: Int) = n % i > 0

    val isPrime = (primes takeWhile squareLE) forall notDivisibleBy
    if (isPrime) primes += n
    else primes
  }
}
