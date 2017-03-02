object PerfectNumbers {
  import NumberType._

  def classify(number: Int): NumberType = {
    val isFactor: Int => Boolean = number % _ == 0

    val candidates = 1 to number/2
    val factors = candidates filter isFactor
    val aliquotSum = factors sum

    if (aliquotSum == number) Perfect
    else if (aliquotSum > number) Abundant
    else Deficient
  }
}

object NumberType {
  sealed trait NumberType

  case object Perfect   extends NumberType
  case object Abundant  extends NumberType
  case object Deficient extends NumberType
}