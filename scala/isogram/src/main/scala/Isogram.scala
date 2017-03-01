object Isogram {
  def isIsogram(word: String): Boolean = {
    val lowerCaseLetters = word filter (_.isLetter) map (_.toLower)
    lowerCaseLetters.distinct.size == lowerCaseLetters.size

//    lowerCaseLetters.foldLeft((true, Set.empty[Char])) {
//      case ((isogram, lettersSoFar), letter) =>
//        (isogram && !lettersSoFar(letter), lettersSoFar + letter)
//    }._1

//    lowerCaseLetters
//      .groupBy(identity)
//      .values forall (_.size == 1)
  }
}
