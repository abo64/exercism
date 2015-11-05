import collection.mutable
import scala.annotation.tailrec

object Prime {
  def nth(n: Int): Int = {
    lazy val lastN = cache.keys max
    def lastPrime = cache(lastN)

    cache getOrElse(n, nextPrime(n, lastN, lastPrime))
  }

  private val cache: mutable.Map[Int,Int] = mutable.Map(1 -> 2)
  private def storePrime(n: Int, prime: Int) = cache += (n -> prime)

  // Sieve of Eratosthenes, but w/o nested Streams to avoid OutOfMemoryError
  @tailrec
  private def nextPrime(n: Int, i: Int, lastPrime: Int): Int = {
    if (n == i) lastPrime
    else {
      val previousPrimes = cache.values
      def isPrime(p: Int) = previousPrimes forall (p % _ != 0)
      val nextPrimes = Stream.from(lastPrime + 1) filter isPrime
      val prime = nextPrimes.head
      storePrime(i + 1, prime)
      nextPrime(n, i + 1, prime)
    }
  }
}