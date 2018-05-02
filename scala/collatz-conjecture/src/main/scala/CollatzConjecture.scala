import scala.annotation.tailrec
import scala.language.postfixOps

object CollatzConjecture {

  def steps(n: Int): Option[Int] = {
    Option(n) filter (_ > 0) map collatzStream

//    Option(n) filter (_ > 0) map collatzRec(0)
  }

    @tailrec
    def collatzRec(step: Int)(m: Int): Int = {
      m match {
        case 1 => step
        case x if even(x) => collatzRec(step + 1)(x / 2)
        case x => collatzRec(step + 1)(x * 3 + 1)
      }
    }


  private def collatzStream(n: Int): Int = {

    val collatzNumbers: Stream[Int] =
      Stream.iterate(n) {
          case x if even(x) => x / 2
          case x => x * 3 + 1
      }

    collatzNumbers takeWhile (_ != 1) size
  }

  private def even(i: Int): Boolean = {
    i % 2 == 0
  }
}
