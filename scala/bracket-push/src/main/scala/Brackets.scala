import scala.annotation.tailrec
import scala.collection.mutable

object Brackets {

  def areBalanced(str: String): Boolean = {
    val openBrackets = mutable.Stack[Char]()

    def matchingOpenBracket(closingBracket: Char): Boolean =
      !openBrackets.isEmpty &&
      openBrackets.pop() == matchingBracket(closingBracket)

    @tailrec
    def loop(chars: List[Char]): Boolean = chars match {
      case Nil => openBrackets.isEmpty
      case x::xs if isOpeningBracket(x) =>
        openBrackets.push(x)
        loop(xs)
      case x::xs if isClosingBracket(x) &&
                    !matchingOpenBracket(x) => false
      case x::xs => loop(xs)
    }

    loop(str.toList)
  }

  private val matchingBracket = Map(']' -> '[', ')' -> '(', '}' -> '{')

  private val isClosingBracket = matchingBracket.keySet
  private val isOpeningBracket = matchingBracket.values.toSet
}
