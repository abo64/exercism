trait Foldable[F[_]] {
  def foldMap[A,B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]): B

  def foldRight[A, B](fa: F[A], z: B)(f: (A, B) => B): B
}

object Foldable {
  implicit val seqFoldable = new Foldable[Seq] {
    override def foldMap[A,B](as: Seq[A])(f: A => B)(implicit mb: Monoid[B]) =
      as.foldLeft(mb.mzero)((b, a) => mb.mappend(b, f(a)))

    override def foldRight[A, B](as: Seq[A], z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
  }

  implicit class InfixFoldable[A, F[_]](fa: F[A])(implicit foldable: Foldable[F]) {
    def foldMap[B](f: A => B)(implicit mb: Monoid[B]): B =
      foldable.foldMap(fa)(f)

    def foldRight[B](z: B)(f: (A, B) => B): B =
      foldable.foldRight(fa, z)(f)
  }
}