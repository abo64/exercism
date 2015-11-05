import scala.annotation.tailrec
import Binary._

class Binary(input: BinaryInput) {
  lazy val toDecimal: BinaryOutput =
    parse(input) getOrElse InvalidInputResult
}

object Binary {
  type BinaryInput = String
  type BinaryOutput = Int

  def apply(input: BinaryInput) = new Binary(input)

  val InvalidInputResult: BinaryOutput = 0

  private def parse(input: BinaryInput): Option[BinaryOutput] = {
    def isValidInput(in: BinaryInput) =
      in.nonEmpty && (in forall (c => c == '0' || c == '1'))

    def toDecimal(in: BinaryInput): BinaryOutput = {
      @tailrec def loop(oct: Seq[Char], result: Int): Int = oct match {
        case Nil => result
        case h +: t => loop(t, result * 2 + h.asDigit)
      }

      loop(in, 0)
    }

    Option(input) filter isValidInput map toDecimal
  }
}