import Deque.Element

class Deque[T] {

  private var first: Option[Element[T]] = None
  private var last: Option[Element[T]] = None

  def unshift(t: T): Unit = {
    val newElement = Some(Element(t, next = first))
    first foreach (_.prev = newElement)
    first = newElement
    last = last orElse first
  }

  def push(t: T): Unit = {
    val newElement = Some(Element(t, prev = last))
    last foreach (_.next = newElement)
    last = newElement
    first = first orElse last
  }

  def shift: Option[T] = {
    val result = first map (_.value)
    first = first flatMap (_.next)
    result
  }

  def pop: Option[T] = {
    val result = last map (_.value)
    last = last flatMap (_.prev)
    result
  }

  def isEmpty: Boolean = first.isEmpty
}

object Deque {
  def apply[T]() = new Deque[T]

  case class Element[T](value: T,
      var next: Option[Element[T]] = None,
      var prev: Option[Element[T]] = None)
}
