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

    def toDecimal(in: BinaryInput): BinaryOutput =
      in.foldLeft(0) {
        case (output, char) => output * 2 + char.asDigit
      }
//      in.reverse.foldRight(0) {
//        case (char, output) => output * 2 + char.asDigit
//      }

    Option(input) filter isValidInput map toDecimal
  }
}