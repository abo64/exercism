object Minesweeper {
  type RowString = String
  type Board = Seq[RowString]
  type AnnotatedBoard = Seq[RowString]

  type Row = Int
  type Col = Int
  type MineCount = Int
  type Square = (Row, Col)
  type SquareContent = Char
  type Annotation = Char

  def annotate(implicit board: Board): AnnotatedBoard = {
    val annotatedSquares = toSquares(board) map (_ map annotate)
    annotatedSquares map (_.mkString)
  }

  private def toSquares(board: Board): Seq[Seq[Square]] = {
    val boardCols = board.headOption map (_.size) getOrElse 0
    val squares =
      for {
        row <- (0 until board.size)
        col <- (0 until boardCols)
      } yield (row, col)

    if (squares.isEmpty) Seq()
    else squares grouped(boardCols) toSeq
  }

  private def annotate(square: Square)(implicit board: Board): Annotation =
    if (isMine(square)) content(square)
    else {
      val mineCount = countMines(square)
      if (mineCount > 0) mineCount.toString()(0)
      else content(square)
    }

  private def adjacentSquares(square: Square)(implicit board: Board): Set[Square] = {
    val (boardRows, boardCols) = (board.size, board.head.size)
    def onBoard(sqr: Square): Boolean = {
      val (row, col) = (sqr._1, sqr._2)
      row >= 0 && row < boardRows && col >= 0 && col < boardCols
    }

    val (r, c) = (square._1, square._2)
    val possibleNeigbors =
      Set((r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1))
    possibleNeigbors filter onBoard
  }

  private def content(square: Square)(implicit board: Board): SquareContent =
    board(square._1)(square._2)

  private val Mine = '*'
  private def isMine(square: Square)(implicit board: Board): Boolean =
    content(square) == Mine

  private def countMines(square: Square)(implicit board: Board): MineCount =
    adjacentSquares(square) count isMine
}
