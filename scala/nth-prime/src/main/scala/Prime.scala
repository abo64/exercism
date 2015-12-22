import collection.mutable
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Prime {
  type Prime = Int

  def nth(n: Int): Prime =
    loop(n, 3, ListBuffer(2))

  // Sieve of Eratosthenes
  @tailrec
  private def loop(n: Int, candidate: Int, primes: ListBuffer[Int]): Prime = {
    if (primes.size == n) primes.last
    else {
      def notDivisibleBy(i: Int) = candidate % i != 0
      def squareLE(i: Int) = i*i <= candidate

      val relevantPrimes = primes takeWhile squareLE
      val candidateIsPrime = relevantPrimes forall notDivisibleBy
      if (candidateIsPrime) primes += candidate
      val nextCandidate = candidate + 1
      loop(n, nextCandidate, primes)
    }
  }
}
