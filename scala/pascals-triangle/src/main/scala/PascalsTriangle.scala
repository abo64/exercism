import scala.annotation.tailrec

object PascalsTriangle {
  type Row = List[Int]
  type PascalsTriangle = List[Row]

  private val FirstRow: Row = List(1)

  def triangle(howManyRows: Int): PascalsTriangle = {
    def nextRow(row: Row): Row =
      zipWith(0 +: row, row :+ 0)(_ + _)

    List.iterate(FirstRow, howManyRows)(nextRow)
  }

  private def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = {
    @tailrec
    def loop(as: List[A], bs: List[B], cs: List[C]): List[C] =
      (as, bs) match {
        case (Nil, Nil) => cs
        case (a+:as, b+:bs) => loop(as, bs, cs :+ f(a, b))
      }

    loop(as, bs, List())
  }
}
