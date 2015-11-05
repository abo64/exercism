import Triangle._
import TriangleType._

case class Triangle(side1: Side, side2: Side, side3: Side) {

  lazy val triangleType: TriangleType = {
    if (!isLogical) Illogical
    else equalSides match {
      case 3 => Equilateral
      case 2 => Isosceles
      case _ => Scalene
    }
  }

  private def equalSides: Int =
    4 - Set(side1, side2, side3).size

  private def isLogical: Boolean = {
    val sidesMustBeGreaterThanZero = side1 > 0 && side2 > 0 && side3 > 0
    val sumOfTwoSidesMustBeGreaterThatThirdSide =
      side1 + side2 > side3 && side2 + side3 > side1 && side1 + side3 > side2

    sidesMustBeGreaterThanZero && sumOfTwoSidesMustBeGreaterThatThirdSide
  }
}

object Triangle {
  type Side = Int
}

object TriangleType {
  sealed trait TriangleType
  object Equilateral extends TriangleType
  object Isosceles extends TriangleType
  object Scalene extends TriangleType
  object Illogical extends TriangleType
}