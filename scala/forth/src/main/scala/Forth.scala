import ForthError.ForthError
import scala.collection.mutable
import scala.util.Try

class Forth extends ForthEvaluator {

  private val state = new ForthState()

  import Forth._

  override def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    val terms = parseText(text)
    val emptyEvaluationState: EvaluationState = Right(new ForthState())
    terms.foldLeft(emptyEvaluationState) {
      (evaluationState, term) => term.evaluate(evaluationState)
    }
  }
}
object Forth {
  def parseText(text: String): Seq[Term] = {
    if (text.trim.isEmpty) Seq()
    else {
      val termStrs = text.split("""[\W&&[^+-/*]]""")
      termStrs map toTerm
    }
  }

  private[this] def toTerm(termStr: String): Term = termStr.toUpperCase match {
    case Number(number) => number
    case Plus(plus) => plus
    case Minus(minus) => minus
    case Mult(mult) => mult
    case Div(div) => div
    case Dup(dup) => dup
    case Drop(drop) => drop
    case Swap(swap) => swap
    case Over(over) => over
  }

  type EvaluationState = Either[ForthError, ForthState]
  type IntResult = Either[ForthError, Int]

  sealed trait Term {
    def evaluate(state: EvaluationState): EvaluationState
  }
  case class Number(number: Int) extends Term {
    override def evaluate(state: EvaluationState): EvaluationState =
      state.right.flatMap (_.push(number))
  }
  object Number {
    def unapply(str: String): Option[Number] =
      Try(Number(str.toInt)).toOption
  }

  trait Operator extends Term {
    val op: String
    def unapply(str: String): Option[this.type] =
      Option(str) filter (_ == op) map (_ => this)
  }

  trait BinaryOp extends Operator {
    val operation: (Int, Int) => IntResult
    override def evaluate(evaluationState: EvaluationState): EvaluationState =
      for {
        state <- evaluationState.right
        arg2 <- state.pop.right
        arg1 <- state.pop.right
        result <- operation(arg1, arg2).right
        newState <- state.push(result).right
      } yield newState
  }

  case object Plus extends BinaryOp {
    override val op = "+"
    override val operation = (arg1: Int, arg2:Int) => Right(arg1 + arg2)
  }

  case object Minus extends BinaryOp {
    override val op = "-"
    override val operation = (arg1: Int, arg2:Int) => Right(arg1 - arg2)
  }

  case object Mult extends BinaryOp {
    override val op = "*"
    override val operation = (arg1: Int, arg2:Int) => Right(arg1 * arg2)
  }

  case object Div extends BinaryOp {
    override val op = "/"
    override val operation = (dividend:Int, divisor:Int) =>
      if (divisor == 0) Left(ForthError.DivisionByZero)
      else Right(dividend / divisor)
  }

  case object Dup extends Operator {
    override val op = "DUP"
    override def evaluate(evaluationState: EvaluationState): EvaluationState =
      for {
        state <- evaluationState.right
        next <- state.pop.right
        state1 <- state.push(next).right
        state2 <- state.push(next).right
      } yield state2
  }

  case object Drop extends Operator {
    override val op = "DROP"
    override def evaluate(evaluationState: EvaluationState): EvaluationState =
      for {
        state <- evaluationState.right
        _ <- state.pop.right
      } yield state
  }

  case object Swap extends Operator {
    override val op = "SWAP"
    override def evaluate(evaluationState: EvaluationState): EvaluationState =
      for {
        state <- evaluationState.right
        first <- state.pop.right
        second <- state.pop.right
        state1 <- state.push(first).right
        state2 <- state.push(second).right
      } yield state2
  }

  case object Over extends Operator {
    override val op = "OVER"
    override def evaluate(evaluationState: EvaluationState): EvaluationState =
      for {
        state <- evaluationState.right
        first <- state.pop.right
        second <- state.pop.right
        state1 <- state.push(second).right
        state2 <- state.push(first).right
        state3 <- state.push(second).right
      } yield state3
  }
}

class ForthState extends ForthEvaluatorState {

  private val stack = mutable.Stack[Int]()

  def push(elem: Int): Right[ForthError, ForthState] = {
    stack.push(elem)
    Right(this)
  }

  def pop: Either[ForthError, Int] =
    if (stack.isEmpty) Left(ForthError.StackUnderflow)
    else Right(stack.pop)

  override def toString = stack.reverse mkString " "
}
