import Sublist._
import scala.annotation.tailrec

class Sublist {
  def sublist[T](l1: List[T], l2: List[T]): SublistResult = {
    if (l1 == l2) Equal
    else if (l2 myContainsSlice l1) Sublist.Sublist
    else if (l1 myContainsSlice l2) Superlist
    else Unequal
  }
}

object Sublist {
  sealed trait SublistResult
  object Equal extends SublistResult
  object Unequal extends SublistResult
  object Sublist extends SublistResult
  object Superlist extends SublistResult

  implicit class ListOps[T](self: List[T]) {
    def myContainsSlice(sublist: List[T]): Boolean = {
      @tailrec def loop(superlist: List[T]): Boolean =
        // next line commented out: Nil containsSlice Nil == true
//      if (superlist.isEmpty) false else
        if (superlist.size < sublist.size) false
        else if (superlist startsWith sublist) true
        else loop(superlist.tail)

      loop(self)
    }
  }
}