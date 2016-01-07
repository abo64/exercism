import ForthError.ForthError
import scala.util.Try
import Forth._


case class ForthState(stack: List[Int] = List(),
    newWords:Map[WordName, WordDefinition] = Map()) extends ForthEvaluatorState
{
  def push(elem: Int): ForthOpResult[Unit] =
    Right(((), copy(stack = elem::stack)))

  // FP style: return value and new state
  def pop: ForthOpResult[Int] =
    if (stack.isEmpty) Left(ForthError.StackUnderflow)
    else Right((stack.head, copy(stack = stack.tail)))

  def addNewWord(wordName: WordName, wordDefinition: WordDefinition): ForthOpResult[Unit] = {
    val newWord = (wordName, wordDefinition)
    Right(((), copy(newWords = newWords + newWord)))
  }

  override def toString = stack.reverse mkString " "
}

class Forth extends ForthEvaluator {
  override def eval(input: String): Either[ForthError, ForthEvaluatorState] =
    evalForth(parseInput(input), EmptyEvaluationState)
}

object Forth {
  type EvaluationState = Either[ForthError, ForthState]
  implicit class EvaluationStateOps(self: EvaluationState) {
    def >>=[A](f: ForthResult): EvaluationState = self.right.flatMap(f)
  }
  val EmptyEvaluationState: EvaluationState = Right(ForthState())

  // FP style: return value and new state
  type ForthOpResult[+A] = Either[ForthError, (A, ForthState)]
  type ForthOp[+A] = ForthState => ForthOpResult[A]
  type ForthResult = ForthState => EvaluationState
  implicit def forthOpToForthResult(forthOp: ForthOp[_]): ForthResult =
    fs =>  forthOp(fs).right.map { case (_, state) => state }

  type ParsedInput = List[String]
  type WordName = String
  type WordDefinition = List[String]

  def evalForth(parsedInput: ParsedInput, state: EvaluationState): EvaluationState =
    parsedInput match {
      case Nil => state
      case ":"::wordName::xs => {
        if (parseNumber(wordName).isDefined) Left(ForthError.InvalidWord)
        else {
          val (newWordDef, _::parsedInputRest) = xs span (_ != ";")
          val nextState = state >>= doAddNewWord(wordName.toUpperCase, newWordDef)
          evalForth(parsedInputRest, nextState)
        }
      }
      case x::xs => {
        val maybeWordDef: Option[WordDefinition] =
          state.fold(_ => None, _.newWords.get(x.toUpperCase))
        val nextParsedInput = maybeWordDef.map (_ ++ xs) getOrElse xs
        val maybeNumber = parseNumber(x)
        val nextState: EvaluationState =
          if (maybeNumber.isDefined) state >>= doPush(maybeNumber.get)
          else if (maybeWordDef.isDefined) state
          else x.toUpperCase match {
            case "+" => state >>= doBinaryOp(_ + _)
            case "-" => state >>= doBinaryOp(_ - _)
            case "*" => state >>= doBinaryOp(_ * _)
            case "/" => state >>= doDiv
            case "DUP" => state >>= doDup
            case "DROP" => state >>= doDrop
            case "SWAP" => state >>= doSwap
            case "OVER" => state >>= doOver
            case _ => Left(ForthError.InvalidWord)
          }

        evalForth(nextParsedInput, nextState)
      }
    }

  def parseInput(input: String): ParsedInput = {
    if (input.trim.isEmpty) List()
    else input.split("""[\W&&[^+-/*:;]]""").filter(_.nonEmpty) toList
  }

  def parseNumber(str: String): Option[Int] =
    Try(str.toInt).toOption

  def map[A,B](s: ForthOp[A])(f: A => B): ForthOp[B] =
    fs =>
      s(fs).right.map {
        case (a, fs1) => (f(a), fs1)
      }

  // for-comprehensions w/ Either have a bug when matching tuples:
  // https://issues.scala-lang.org/browse/SI-5589
  // so I had to go for flatMap instead
  def flatMap[A,B](f: ForthOp[A])(g: A => ForthOp[B]): ForthOp[B] =
    fs =>
      f(fs).right.flatMap {
        case (a, fs1) => g(a)(fs1)
      }

  def doPush(elem: Int): ForthOp[Unit] = _.push(elem)
  def doPop: ForthOp[Int] = _.pop
  def doAddNewWord(wn: WordName, wd: WordDefinition): ForthOp[Unit] = _.addNewWord(wn, wd)

  def doBinaryOp(op: (Int,Int) => Int): ForthResult =
    flatMap(doPop) { x =>
      flatMap(doPop) { y =>
        doPush(op(y, x))
      }
    }

  def doDiv: ForthResult =
    flatMap(doPop) { y =>
      flatMap(doPop) { z =>
        if (y != 0) doPush(z / y)
        else _ => Left(ForthError.DivisionByZero)
      }
    }

  def doDup: ForthResult =
    flatMap(doPop) { y =>
      flatMap(doPush(y)) { _ =>
        doPush(y)
      }
    }

  def doDrop: ForthResult = doPop

  def doSwap: ForthResult =
    flatMap(doPop) { x =>
      flatMap(doPop) { y =>
        flatMap(doPush(x)) { _ =>
          doPush(y)
        }
      }
    }

  def doOver: ForthResult =
    flatMap(doPop) { x =>
      flatMap(doPop) { y =>
        flatMap(doPush(y)) { _ =>
          flatMap(doPush(x)) { _ =>
            doPush(y)
          }
        }
      }
    }
}

