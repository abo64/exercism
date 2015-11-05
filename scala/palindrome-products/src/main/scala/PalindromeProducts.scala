import PalindromeProducts._

class PalindromeProducts(start: Int, end: Int) {

  private lazy val range = (start to end)

  lazy val allPalindromeProducts: Seq[PalindromeProduct] = {
    val valuesWithFactors: Seq[(Value, Factors)] =
      for {
        i <- range
        j <- range if isPalindrome(i * j)
        value = i * j
        factors = (i,j)
      } yield (value, factors)

    val toFactors: PartialFunction[(Value,Factors),Factors] =
      { case (_, (a,b)) => if (a <= b) (a,b) else (b, a) }

    val valuesWithAllFactors: Map[Value, AllFactors] =
      valuesWithFactors groupBy (_._1) mapValues { _ map toFactors toSet }

    valuesWithAllFactors toSeq
  }

  lazy val smallest: PalindromeProduct = {
    allPalindromeProducts min
  }

  lazy val largest: PalindromeProduct = {
    allPalindromeProducts max
  }
}

object PalindromeProducts {
  def apply(start: Int, end: Int) = new PalindromeProducts(start, end)

  type Value = Int
  type Factors = (Int,Int)
  type AllFactors = Set[Factors]
  type PalindromeProduct = (Value, AllFactors)

  implicit val PalindromeProductOrdering: Ordering[PalindromeProduct] =
    Ordering.by(_._1)

  def isPalindrome(n: Int): Boolean = {
    val nStr = n.toString
    nStr == nStr.reverse
  }
}
