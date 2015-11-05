import Matrix._

class Matrix(matrix: MatrixRepr) {
  def saddlePoints: Points =
    coordinates filter isSaddlePoint

  private def isSaddlePoint(coordinate: Coordinate): Boolean = {
    val elt = element(coordinate)
    val (x, y) = coordinate
    row(coordinate).max == elt && col(coordinate).min == elt
  }

  private def coordinates: Set[Coordinate] =
    (for {
      x <- (0 until matrix.size)
      y <- (0 until matrix(0).size)
    } yield (x,y)) toSet

  private def element(coordinate: Coordinate): Element =
    row(coordinate)(coordinate._2)

  private def row(coordinate: Coordinate): Row =
    matrix(coordinate._1)

  private def col(coordinate: Coordinate): Col =
    matrix map (_(coordinate._2))
}

object Matrix {
  type Coordinate = (Int,Int)
  type Element = Int
  type Points = Set[Coordinate]
  type Row = Seq[Element]
  type Col = Seq[Element]
  type MatrixRepr = Seq[Row]

  def apply(matrix: MatrixRepr) = new Matrix(matrix)
}