import Relations._
import RelationsAsync._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class Relations[T](f: RelationSource[T]) {
  def relation(t1: T, t2: T, depth: Int = 1): RelationResult = {
    def loop(depth: Int, ts: Related[T]): RelationResult =
      if (ts contains t2) true
      else if (depth == 0) false
      else loop(depth - 1, ts flatMap f)

    loop(depth, f(t1))
  }
}

class RelationsAsync[T](f: RelationSourceAsync[T]) {
  def relationAsync(t1: T, t2: T, depth: Int = 1)(implicit ec: ExecutionContext): RelationResultAsync = {
    def loop(depth: Int)(fts: RelatedAsync[T]): RelationResultAsync =
      fts flatMap { ts: Related[T] =>
        if (ts contains t2) Future.successful(true)
        else if (depth == 0) Future.successful(false)
        else {
          val sfts: Set[Future[Related[T]]] = ts map f
          val sfbs: Set[RelationResultAsync] = sfts map loop(depth - 1)
          Future.sequence(sfbs) map (_ exists identity)
        }
    }
    loop(depth)(f(t1))
  }
}

object Relations {
  type Related[T] = Set[T]
  type RelationSource[T] = T => Related[T]
  type RelationResult = Boolean

  def apply[T](f: RelationSource[T]) = new Relations(f)
}

object RelationsAsync {
  type RelatedAsync[T] = Future[Related[T]]
  type RelationSourceAsync[T] = T => Future[Related[T]]
  type RelationResultAsync = Future[RelationResult]

  def apply[T](f: RelationSourceAsync[T]) = new RelationsAsync(f)
}