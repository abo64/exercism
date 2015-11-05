class Year(year: Int) {
  def isLeap: Boolean = {
    def isEvenlyDivisibleBy(divisor: Int): Boolean = year % divisor == 0

    isEvenlyDivisibleBy(4) &&
    (!isEvenlyDivisibleBy(100) || isEvenlyDivisibleBy(400))
  }
}

object Year {
  // alternatively declare Year as case class
  def apply(year: Int) = new Year(year)
}
