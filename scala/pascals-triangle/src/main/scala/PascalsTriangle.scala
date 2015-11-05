import scala.annotation.tailrec

object PascalsTriangle {
  type Row = Seq[Int]
  type PascalsTriangle = Seq[Row]

  def triangle(rows: Int): PascalsTriangle = {
    @tailrec def loop(row: Int, result: PascalsTriangle): PascalsTriangle = {
      if (row == 1) result
      else loop(row - 1, result :+ nextRow(result last))
    }

    loop(rows, FirstRow)
  }

  private val FirstRow: PascalsTriangle = (Seq(Seq(1)))

  private def nextRow(row: Row): Row = {
    def sumOfNeighbors(position: Int): Int = {
      val left = if (position == 0) 0 else row(position - 1)
      val right = if (position < row.size) row(position) else 0
      left + right
    }

    (0 to row.size) map sumOfNeighbors
  }
}
