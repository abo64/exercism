import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.DurationLong
import scala.util.Random

package object future {
  type GroupPK = Long
  case class Group(groupPK: GroupPK)
  type UserPK = Long
  case class User(userPK: UserPK)

  private val SleepingPeriod = 3 second

  private def sleep = Thread.sleep(SleepingPeriod.toMillis)

  def findGroup(groupPK: GroupPK): Future[Group] =
    noisyAsync("findGroup") { sleep; Group(groupPK) }

  def findMembers(groupPK: GroupPK): Future[Seq[User]] =
    noisyAsync("findMembers") {
      sleep
      throw new Exception("oops!")
      (0 until Random.nextInt(9)) map (User(_))
    }

  def findModeratedGroups(moderatorPK: UserPK): Future[Seq[GroupPK]] =
    noisyAsync("findModeratedGroups") {
      sleep
      (0 until Random.nextInt(9) map (_.toLong))
    }

  def findGroupAndMembers(groupPK: GroupPK): Future[(Group,Seq[User])] = {
    val group = findGroup(groupPK)
    val members = findMembers(groupPK)
    for {
      g <- group
      ms <- members
    } yield (g, ms)

    // this solution will not process in parallel
//    for {
//      group <- findGroup(groupPK)
//      members <- findMembers(groupPK)
//    } yield (group, members)
  }

  def findModeratedMembers(moderatorPK: UserPK): Future[Seq[User]] = {
    for {
      moderatedGroups <- findModeratedGroups(moderatorPK)
      memberSeqs = Future.sequence(moderatedGroups map findMembers)
      members <- memberSeqs.map (_.flatten)
    } yield members
  }

  def async[T](block: => T): Future[T] =
    Future(block)

  def noisyAsync[T](name: String)(block: => T): Future[T] = {
    val start = System.currentTimeMillis
    def duration = (System.currentTimeMillis - start) milliseconds
    val noisy: PartialFunction[Any,Unit] = {
      case any: Any => println(s"${Thread.currentThread.getName} $name: $any [$duration]")
    }

    val future = async { block }
    future onSuccess noisy
    future onFailure noisy
    future
  }

  def await[T](f: Future[T]): T =
    Await.result(f, 1 second)
}
