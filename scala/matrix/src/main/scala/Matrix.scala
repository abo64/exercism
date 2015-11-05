import Matrix._

case class Matrix(rows: Rows[Int], cols: Cols[Int])

object Matrix {
  type MatrixVector[T] = Seq[T]
  type Rows[T] = Seq[MatrixVector[T]]
  type Cols[T] = Seq[MatrixVector[T]]

  private val RowSeparator = '\n'
  private val CellSeparator = ' '

  def apply(str: String): Matrix = {
    val rowsAsStrings: Seq[String] = str split RowSeparator toSeq
    val rows: Rows[Int] =
      rowsAsStrings map (_ split CellSeparator map (_.toInt) toSeq)
    val cols: Cols[Int] = rows.transpose

    Matrix(rows, cols)
  }
}
