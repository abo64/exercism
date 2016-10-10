import scala.annotation.tailrec

object Hamming {
  def compute[T](xs: Seq[T], ys: Seq[T]): Option[Int] =
    zipWithExactMay(xs, ys)(_ != _) map (_.count(identity))
    // Haskell: genericLength . filter id <$> zipWithExactMay (/=) xs ys
    // (Haskell doesn't have a "count" function)

  private def zipWithExactMay[A,B,C](as: Seq[A], bs: Seq[B])(f: (A,B) => C): Option[Seq[C]] = {
    @tailrec
    def loop(as: Seq[A], bs: Seq[B], cs: Seq[C]): Option[Seq[C]] =
      (as, bs) match {
        case (Nil, Nil) => Some(cs)
        case (a+:as, b+:bs) => loop(as, bs, cs :+ f(a, b))
        case _ => None
      }

    loop(as, bs, Seq())
  }
}
