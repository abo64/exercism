import Queens._
import Chessboard._

object Queens {

  // needed for the tests to compile and run
  def apply() = this

  type Queen = Option[Position]

  def boardString(whiteQueen: Queen, blackQueen: Queen): Chessboard = {
    def getSquare(position: Position): Square = {
      def queenSquare(queen: Queen, square: Square): Option[Square] =
        queen filter (_ == position) map (_ => square)

      queenSquare(whiteQueen, WhiteQueenSquare) orElse
      queenSquare(blackQueen, BlackQueenSquare) getOrElse EmptySquare
    }

    val squares: Squares =
      for {
        row <- Rows
        col <- Columns
        square = getSquare(Position(row, col))
      } yield square

    val rows: Seq[Squares] = squares.grouped(BoardSize).toSeq
    val rowsAsStrings: Seq[String] = rows map (_.mkString("", " ", "\n"))
    rowsAsStrings mkString
  }

  def canAttack(position1: Position, position2: Position): Boolean = {
    val deltaRow = math.abs(position1.row - position2.row)
    val deltaCol = math.abs(position1.column - position2.column)

    def onSameRow = deltaRow == 0
    def onSameCol = deltaCol == 0
    def onSameDiagonal = deltaRow == deltaCol

    onSameRow || onSameCol|| onSameDiagonal
  }
}

case class Position(row: Row, column: Column)

object Chessboard {
  type Chessboard = String
  type Square = Char
  type Squares = Seq[Square]

  type Row = Int
  type Column = Int

  val BoardSize = 8

  val Rows = (0 until BoardSize)
  val Columns = (0 until BoardSize)

  val EmptySquare: Square = '_'
  val WhiteQueenSquare: Square = 'W'
  val BlackQueenSquare: Square = 'B'
}
