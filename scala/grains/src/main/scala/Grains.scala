import collection.mutable
import GrainsTypes._

object GrainsTypes {
  type Grains = BigInt
  type ChessboardSquares = Int
}

object Grains extends WithCache[ChessboardSquares,Grains] {

  val TotalChessboardSquares = 64

  val Two = BigInt(2)

  def square(chessboardSquares: ChessboardSquares): Grains = {
    require(chessboardSquares >= 1 && chessboardSquares <= TotalChessboardSquares,
        s"invalid chessboard squares: $chessboardSquares")

    withCache(chessboardSquares, grains)
  }

  def total: Grains = (1 to TotalChessboardSquares) map square sum

  private def grains(chessboardSquares: ChessboardSquares): Grains =
    Two pow (chessboardSquares - 1)
}

trait WithCache[K,V] {
  private val cache = mutable.Map[K,V]()

  def withCache(key: K, f: K => V): V =
    cache getOrElseUpdate(key, f(key))
}
