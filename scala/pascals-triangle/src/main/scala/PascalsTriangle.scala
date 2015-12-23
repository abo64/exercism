object PascalsTriangle {
  type Row = Seq[Int]
  type PascalsTriangle = Seq[Row]

  def triangle(rows: Int): PascalsTriangle = {
    unfoldRight((rows, FirstRow)) { case (rowNumber, lastRow) =>
      if (rowNumber == 0) None
      else {
        val newRow = nextRow(lastRow)
        Some((lastRow, (rowNumber - 1, newRow)))
      }
    }
  }

  private val FirstRow: Row = (Seq(1))

  private def nextRow(row: Row): Row = {
    def sumOfNeighbors(position: Int): Int = {
      val left = if (position == 0) 0 else row(position - 1)
      val right = if (position < row.size) row(position) else 0
      left + right
    }

    (0 to row.size) map sumOfNeighbors
  }

  private[this] def unfoldRight[A,B](seed: B)(f: B => Option[(A,B)]): Seq[A] =
    f(seed) match {
    case None => Seq()
    case Some((a,b)) => a +: unfoldRight(b)(f)
  }
}
