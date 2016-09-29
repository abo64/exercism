import scala.annotation.tailrec

object Hamming {
  def compute[T](xs: Seq[T], ys: Seq[T]): Option[Int] = {
    def difference(x: T, y: T): Int = if (x != y) 1 else 0

    @tailrec
    def loop(xs: Seq[T], ys: Seq[T], acc: Int): Option[Int] =
      (xs, ys) match {
        case (Nil, Nil) => Some(acc)
        case (a+:as, b+:bs) => loop(as, bs, acc + difference(a, b))
        case _ => None
      }

    loop(xs, ys, 0)
  }
}
