import scala.annotation.tailrec

object BinarySearch {
  def search[T <% Ordered[T]](ts: Seq[T], what: T): Option[Int] =
  {
    @tailrec
    def loop(start: Int, end: Int): Option[Int] =
        if (start < 0 || end <= start) None
        else {
          val pivot = (end + start) / 2
          val found = ts(pivot)
          if (found == what) Some(pivot)
          else if (found > what) loop(start, pivot)
          else loop(pivot + 1, end)
        }

    loop(0, ts.length)
  }
}
