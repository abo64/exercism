import scala.annotation.tailrec

class Accumulate {
  def accumulate[A,B](f: A => B, as: List[A]): List[B] = {
//    as.foldRight(List.empty[B]){(a, bs) => f(a) :: bs}

    // just for fun in "Haskell style"
    // Haskell is (not really surprisingly) more elegant here:
    // accumulate f = foldr ((:) . f) []
    val cons: B => (List[B] => List[B]) = b => (b :: _)
    as.foldRight(List.empty[B])(Function.uncurried(cons compose f))
  }
}
