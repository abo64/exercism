import scala.annotation.tailrec

trait SimpleLinkedList[T] {
  def isEmpty: Boolean
  def value: T
  def add(item: T): SimpleLinkedList[T]
  def next: SimpleLinkedList[T]
  def reverse: SimpleLinkedList[T]
  def toSeq: Seq[T]
}

  // objects cannot be parameterized
case object Empty extends SimpleLinkedList[Any] {
  def apply[T]() = this.asInstanceOf[SimpleLinkedList[T]]
  def unapply[T](xs: SimpleLinkedList[T]) = xs == this

  override def isEmpty: Boolean = true
  override def value = throw new Exception("value called on Empty")
  override def add(item: Any): SimpleLinkedList[Any] = SimpleLinkedList(item)
  override def next = throw new Exception("next called on Empty")
  override def reverse: SimpleLinkedList[Any] = this
  override def toSeq: Seq[Any] = Seq.empty
}

case class Cons[T](override val value: T,
    override val next: SimpleLinkedList[T]) extends SimpleLinkedList[T]
{
  override def isEmpty: Boolean = false

  override def add(item: T): SimpleLinkedList[T] =
    Cons(item, reverse).reverse

  override def reverse: SimpleLinkedList[T] = {
    @tailrec
    def loop(xs: SimpleLinkedList[T], ys: SimpleLinkedList[T]): SimpleLinkedList[T] =
      xs match {
        case Empty() => ys
        case Cons(x, xs) => loop(xs, Cons(x, ys))
      }

    loop(this, Empty())
  }

  override def toSeq: Seq[T] = value +: next.toSeq
}

object SimpleLinkedList {
  def apply[T](ts: T*): SimpleLinkedList[T] = fromSeq(ts)

  def fromSeq[T](ts: Seq[T]): SimpleLinkedList[T] =
    ts.foldRight(SimpleLinkedList.empty[T])(Cons(_, _))

  def empty[T]: SimpleLinkedList[T] = Empty[T]()
}