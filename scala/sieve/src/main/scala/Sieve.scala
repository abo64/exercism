object Sieve {
  type Prime = Int

  def primesUpTo(n: Int): Seq[Prime] =
    primes takeWhile (_ <= n)

  private val primes: Stream[Int] = {
    def sqrLessEqualThan(j: Int, i: Int) = j * j <= i
    def notDivisibleBy(i: Int, k: Int) = i % k > 0

    2 #:: Stream.from(3, 2).filter { i =>
      primes.takeWhile(sqrLessEqualThan(_, i))
            .forall(notDivisibleBy(i, _))
    }
  }
}
