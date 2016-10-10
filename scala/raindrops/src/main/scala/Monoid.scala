trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}

object Monoid {
  implicit val stringMonoid = new Monoid[String] {
    override def mappend(a1: String, a2: String) = a1 + a2
    override val mzero = ""
  }

  implicit def optionMonoid[A](implicit ma: Monoid[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override def mappend(x: Option[A], y: Option[A]) = (x, y) match {
        case (Some(a1), Some(a2)) => Some(a1 mappend a2)
        case (_, None) => x
        case _ => y
      }
      override val mzero = None
    }

  implicit class InfixMappend[A](a: A)(implicit ma: Monoid[A]) {
    def mappend(b: A): A = ma.mappend(a, b)
  }
}