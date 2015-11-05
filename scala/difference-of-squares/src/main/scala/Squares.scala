class Squares {
  def squareOfSums(n: Int): Int = {
    val sum = (1 to n).sum
    sqr(sum)
  }

  def sumOfSquares(n: Int): Int = {
    val squares = (1 to n) map sqr
    squares.sum
  }

  def difference(n: Int): Int =
    squareOfSums(n) - sumOfSquares(n)

  private def sqr(n: Int) = n * n
}

object Squares {
  def apply() = new Squares
}
