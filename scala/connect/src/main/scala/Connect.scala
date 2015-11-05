import scala.annotation.tailrec
import Connect._
import Color._

class Connect(board: Board) {
  def result: Option[Color] =
    Option(board) filter validBoard flatMap connect
}

object Connect {
  type Board = Seq[String]
  type Field = Char
  type Row = Int
  type Col = Int
  type Coordinate = (Row, Col)

  def apply(board: Board) = new Connect(board)

  private val EmptyField: Field = '.'

  private def validBoard(board: Board) = {
    def isValidField(field: Field) =
      field == White.field || field == Black.field || field == EmptyField

    board forall (_ forall isValidField)
  }

  private def connect(board: Board): Option[Color] = {
    implicit val b = board
    def winner(color: Color): Option[Color] = Option(color) filter wins

    winner(White) orElse winner(Black)
  }

  private def wins(color: Color)(implicit board: Board): Boolean = {
    @tailrec
    def loop(current: Set[Coordinate], visited: Set[Coordinate]): Boolean = {
      if (current exists color.isGoal) true
      else if (current.isEmpty) false
      else {
        val nextVisited = visited ++ current
        val next = current flatMap neighbors(color) diff nextVisited
        loop(next, nextVisited)
      }
    }

    loop(color.start, Set())
  }

  def occupies(color: Color)(coord: Coordinate)(implicit board: Board): Boolean = {
    val (row,col) = coord
    row >= 0 && row < board.size &&
    col >= 0 && col < board(0).size &&
    board(row)(col) == color.field
  }

  private def neighbors(color: Color)(coord: Coordinate)(implicit board: Board): Set[Coordinate] = {
    val (row,col) = coord
    Set((row - 1, col), (row - 1, col + 1), (row, col + 1),
        (row + 1, col), (row + 1, col - 1), (row, col - 1)) filter occupies(color)
  }
}

object Color {
  sealed trait Color {
    val field: Field
    def startCoordinates(implicit board: Board): Set[Coordinate]
    def start(implicit board: Board): Set[Coordinate] =
      startCoordinates filter occupies(this)
    def isGoal(coord: Coordinate)(implicit board: Board): Boolean
  }
  case object White extends Color {
    override val field: Field = 'O'
    override def startCoordinates(implicit board: Board): Set[Coordinate] =
      (0 to board(0).size - 1) map ((0,_)) toSet
    override def isGoal(coord: Coordinate)(implicit board: Board): Boolean =
      coord._1 == board.size - 1
  }
  case object Black extends Color {
    override val field: Field = 'X'
    override def startCoordinates(implicit board: Board): Set[Coordinate] =
      (0 to board.size - 1) map ((_,0)) toSet
    override def isGoal(coord: Coordinate)(implicit board: Board): Boolean =
      coord._2 == board(0).size - 1
  }
}