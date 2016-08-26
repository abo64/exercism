import java.util.concurrent.Executors
import concurrent.{ExecutionContext, Await, Future}
import concurrent.duration.DurationInt

object Frequency {
  type CharFrequency = Map[Char,Int]

  def frequency(threads: Int, text: Seq[String]): CharFrequency = {
    implicit val executionContext =
      ExecutionContext.fromExecutor(Executors.newWorkStealingPool(threads))

    val frequencyFutures = text map charFrequencyAsync
    val futureFrequencies = Future.fold(frequencyFutures)(EmptyCharFrequency)(addFrequencies)
    Await.result(futureFrequencies, 5 seconds)
  }

  private def charFrequencyAsync(line: String)(implicit executor: ExecutionContext): Future[CharFrequency] =
    processAsync(charFrequency(line))

  private def processAsync[T](block: => T)(implicit executor: ExecutionContext): Future[T] =
    Future(block)

  private def charFrequency(line: String): CharFrequency = {
//    println(s"${Thread.currentThread.getName} - processing line: $line")
    filterLine(line) groupBy identity mapValues (_.size)
  }

  private val EmptyCharFrequency: CharFrequency = Map.empty

  private val IgnoredChars = """\p{Punct}|\p{Digit}|\p{Space}"""
  private def filterLine(line: String): String =
    line replaceAll(IgnoredChars, "") toLowerCase

  private def addFrequencies(map1: CharFrequency, map2: CharFrequency) =
    map1 ++ map2.map{ case (k,v) => k -> (v + map1.getOrElse(k,0)) }
}
