import Bst.OrderedValues

case class Bst[T: OrderedValues](value: T,
    left: Option[Bst[T]] = None, right: Option[Bst[T]] = None)
{
  type Node = Option[Bst[T]]

  def insert(newValue: T): Bst[T] = {
    def insertNode(node: Node, insertF: Node => Bst[T]): Bst[T] = {
      val newNode: Node = node map (_.insert(newValue)) orElse Some(Bst(newValue))
      insertF(newNode)
    }

    if (newValue <= value) insertNode(left, n => copy(left = n))
    else insertNode(right, n => copy(right = n))
  }

  def foldLeft[A](z: A)(f: (A,T) => A): A = {
    val leftA = left.fold(z)(_.foldLeft(z)(f))
    val a = f(leftA, value)
    right.fold(a)(_.foldLeft(a)(f))
  }
}

object Bst {
  type OrderedValues[T] = T => Ordered[T]

  def fromList[T: Ordering](ts: List[T]): Bst[T] = {
    require(ts.nonEmpty, "list must be nonEmpty")
    ts.tail.foldLeft(Bst(ts.head))((bst, t) => bst insert t)
  }

  def toList[T](bst: Bst[T]): List[T] =
    bst.foldLeft(List.empty[T])((l,t) => l :+ t)
}