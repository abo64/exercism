import Phrase.WordSeparators
import scala.collection.mutable

class Phrase(phrase: String) {
  lazy val wordCount: Map[String,Int] = {
    type Counts = mutable.Map[String,Int]

    def incrementCount(counts: Counts, word: String): Counts = {
      counts(word) = counts.getOrElse(word, 0) + 1
      counts
    }

    val caseInsensitiveWords = phrase split WordSeparators map (_.toLowerCase)
    caseInsensitiveWords.foldLeft(mutable.Map[String,Int]())(incrementCount) toMap
  }

}

object Phrase {
  // got that from navaro1 - sweet!
  val WordSeparators = """[^\w']+"""
}
