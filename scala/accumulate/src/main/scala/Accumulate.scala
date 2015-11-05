import scala.annotation.tailrec

class Accumulate {
  def accumulate[A,B](f: A => B, as: List[A]): List[B] =
    as.foldLeft(List.empty[B]){(bs,a) => bs :+ f(a)}
}
