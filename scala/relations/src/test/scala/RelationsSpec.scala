import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures

class RelationsSpec extends FlatSpec with ScalaFutures with IntegrationPatience {

  behavior of "relation"

  it should "find an immediate relation" in {
    val relations = Relations(Map("Lucy" -> Set("Linus")) withDefaultValue NoRelationsFound)
    assert(relations.relation("Lucy", "Linus") == true)
    assert(relations.relation("Lucy", "Lucy") == false)
    assert(relations.relation("Linus", "Lucy") == false)
  }

  it should "find a transitive relation" in {
    val relations =
      Relations(Map(1 -> Set(2,3,4), 2 -> Set(3,4,5),
          5 -> Set(8,9,10)) withDefaultValue NoRelationsFound)
    assert(relations.relation(1, 10) == false)
    assert(relations.relation(1, 10, depth = 1) == false)
    assert(relations.relation(1, 10, depth = 2) == true)
  }

  it should "pass along errors" in {
    val exception = new Exception("oops!")
    val oops: Int => Set[Int] =
      i =>
        if (i == 1) Set(2,3,4)
        else if (i == 3) throw exception
        else NoRelationsFound
    val relations = Relations(oops)
    val result = intercept[Exception] { relations.relation(1, 5) }
    assert(result == exception)
  }

  behavior of "relationAsync"

  it should "eventually find an immediate relation" in {
    val relations = RelationsAsync(Map("Lucy" -> Set("Linus")))
    assertRelationAsync(relations.relationAsync("Lucy", "Linus"), true)
    assertRelationAsync(relations.relationAsync("Lucy", "Lucy"), false)
    assertRelationAsync(relations.relationAsync("Linus", "Lucy"), false)
  }

  it should "eventually find a transitive relation" in {
    val relations = RelationsAsync(Map(1 -> Set(2,3,4), 2 -> Set(3,4,5), 5 -> Set(8,9,10)))
    assertRelationAsync(relations.relationAsync(1, 10), false)
    assertRelationAsync(relations.relationAsync(1, 10, depth = 1), false)
    assertRelationAsync(relations.relationAsync(1, 10, depth = 2), true)
  }

  it should "pass along errors" in {
    val exception = new Exception("oops!")
    val oops: Int => Set[Int] =
      i =>
        if (i == 1) Set(2,3,4)
        else if (i == 3) throw exception
        else NoRelationsFound
    val relations = RelationsAsync(oops)
    val result =
      relations.relationAsync(1, 5) recover { case e if e == exception => }
    whenReady(result)(_ => ())
  }

  private val random = new Random

  private def NoRelationsFound[T] = Set[T]()

  def toFuture[T](ts: Set[T]) = {
    val delay = random.nextInt(100)
    Thread.sleep(delay); Future.successful(ts)
  }

  private def assertRelationAsync(result: Future[Boolean], expected: Boolean) =
    whenReady(result)(r => assert(r == expected))

  private implicit def mapToRelationSource[T](m: Map[T,Set[T]]): T => Future[Set[T]] =
    functionToRelationsSource(m withDefaultValue NoRelationsFound)

  private implicit def functionToRelationsSource[T](f: T => Set[T]): T => Future[Set[T]] =
    t => toFuture(f(t))
}