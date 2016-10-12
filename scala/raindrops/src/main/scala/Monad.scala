trait Monad[M[_]] {
  def unit[A](a: => A): M[A]

  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def filter[A](ma: M[A])(p: A => Boolean): M[A]
}

object Monad {
  implicit val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    override def flatMap[A,B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
    override def filter[A](ma: Option[A])(p: A => Boolean) = ma filter p
  }

  def unit[A, M[_]](a: A)(implicit monad: Monad[M]): M[A] = monad.unit(a)

  implicit class MonadOps[A, M[_]](ma: M[A])(implicit monad: Monad[M]) {
//    def unit[B](b: B): M[B] = monad.unit(b)
    def map[B](f: A => B): M[B] = monad.map(ma)(f)
    def filter(p: A => Boolean): M[A] = monad.filter(ma)(p)

    def >>=[B](f: A => M[B]): M[B] = monad.flatMap(ma)(f)
    def >>[B](mb: M[B]): M[B] = ma >>= const(mb)
    def guard(p: => Boolean): M[Unit] = for (_ <- ma if p) yield (())

    private def const[A,B](a: A)(ignore: B): A = a
  }
}
