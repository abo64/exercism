trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}

object Monoid {
  case class IntSum(getSum: Int) {
    def +(that: IntSum): IntSum = IntSum(this.getSum + that.getSum)
  }

  implicit val intSumMonoid = new Monoid[IntSum] {
    override def mappend(a1: IntSum, a2: IntSum): IntSum = a1 + a2
    override val mzero = IntSum(0)
  }

  implicit class InfixMappend[A](a: A)(implicit ma: Monoid[A]) {
    def mappend(b: A): A = ma.mappend(a, b)
  }
}