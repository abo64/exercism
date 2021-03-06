import java.util.concurrent.Executors

import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.ParSeq
import scala.concurrent.{ExecutionContext, Await, Future}
import scala.concurrent.duration.DurationInt
import scala.concurrent.forkjoin.ForkJoinPool

object Frequency {
  type CharFrequency = Map[Char,Int]

  def frequency(threads: Int, text: Seq[String]): CharFrequency = {
//    seqFrequency(text)
    parFrequency(threads, text)
//    futureFrequency(threads, text)
  }

  // sequential solution
  private def seqFrequency(text: Seq[String]): CharFrequency = {
    val lineFrequencies: Seq[CharFrequency] =
      text map charFrequency
    lineFrequencies.fold(Map[Char,Int]())(addFrequencies)
  }

  // using parallel collections
  private def parFrequency(threads: Int, text: Seq[String]): CharFrequency = {
    val parText: ParSeq[String] =
      kestrel (text.par) {
        _.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(threads))
      }

    val lineFrequencies: ParSeq[CharFrequency] =
      parText map charFrequency
    lineFrequencies.fold(Map[Char,Int]())(addFrequencies)
  }

  // using scala.concurrent.Future
  private def futureFrequency(threads: Int, text: Seq[String]): CharFrequency = {
    implicit val executionContext =
      ExecutionContext.fromExecutor(Executors.newWorkStealingPool(threads))

    val frequencyFutures: Seq[Future[CharFrequency]] =
      text map charFrequencyAsync
    val futureFrequencies: Future[CharFrequency] = 
      Future.fold(frequencyFutures)(EmptyCharFrequency)(addFrequencies)

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

  // kestrel combinator for side effect
  private def kestrel[A](a: A)(f: A => Unit): A = { f(a); a }
}
