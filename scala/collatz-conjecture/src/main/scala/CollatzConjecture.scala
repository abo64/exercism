object CollatzConjecture {

  def steps(n: Int): Option[Int] = {

    def even(i: Int): Boolean = {
      i % 2 == 0
    }

    def collatz(step: Int)(m: Int): Int = m match {
      case 1 => step
      case x if even(x) => collatz(step + 1)(x / 2)
      case x => collatz(step + 1)(x * 3 + 1)
    }

    Option(n) filter (_ > 0) map collatz(0)
  }
}
