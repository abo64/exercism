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
//    this match {
//      case Bst(t, None, None) => f(z,t)
//      case Bst(t, Some(l), None) => f(l.foldLeft(z)(f),t)
//      case Bst(t, None, Some(r)) => r.foldLeft(f(z,t))(f)
//      case Bst(t, Some(l), Some(r)) => {
//        val leftA = f(l.foldLeft(z)(f),t)
//        r.foldLeft(leftA)(f)
//      }
//    }
  }

  def foreach[U](f: T => U): Unit = {
    left foreach (_.foreach(f))
    f(value)
    right foreach (_.foreach(f))
//    this match {
//      case Bst(t, None, None) => f(t)
//      case Bst(t, Some(l), None) => { l.foreach(f); f(t) }
//      case Bst(t, None, Some(r)) => { f(t); r.foreach(f) }
//      case Bst(t, Some(l), Some(r)) => { l.foreach(f); f(t); r.foreach(f) }
//    }
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

//  def toList[T <% Ordered[T]](bst: Bst[T]): List[T] = {
//    import scala.collection.mutable.ListBuffer
//    val lb = ListBuffer[T]()
//    bst foreach (lb += _)
//    lb toList
//  }
}