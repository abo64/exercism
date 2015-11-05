import scala.util.Try

object WordProblem {
  type WordProblem = String
  type Answer = Option[Int]

  private val WordProblem = """What is (.+)\?""".r

  def apply(wordProblem: WordProblem): Answer = wordProblem match {
    case WordProblem(expr) => {
      val expressionStrs = expr split " (by |to the )?" toSeq
      val expressions = expressionStrs map (_.toExpression)
      val reducedExpr =
        expressions.foldLeft(Zero: Expression)((soFar, expr) => expr(soFar))
      reducedExpr.eval
    }
    case _ => None
  }

  implicit class StringOps(self: String) {
    def toExpression: Expression = self match {
      case Number(n) => n
      case Plus(p) => p
      case Minus(m) => m
      case Multiplied(m) => m
      case Divided(m) => m
      case Raised(m) => m
      case _ => throw new Exception(s"invalid expression: $self")
    }
  }
}

sealed trait Expression extends ((Expression) => Expression) {
  def eval: Option[Int]
}
object Zero extends Expression {
  override def apply(expr: Expression): Expression = expr
  override val eval = None
}

case class Number(number: Int) extends Expression {
  override def apply(expr: Expression): Expression = expr(this)
  override val eval = Some(number)
}
object Number {
  def unapply(str: String): Option[Number] =
    Try(Number(str.toInt)).toOption
}

object Operation {
  type Op = (Int,Int) => Int
}
import Operation._
trait Operation extends Expression {
  val op: Op
  val left: Option[Expression] = None
  val right: Option[Expression] = None
  override def eval =
    for {
      l <- left map (_.eval)
      lValue <- l
      r <- right map (_.eval)
      rValue <- r
    } yield op(lValue, rValue)

  override def apply(expr: Expression): Expression =
    if (left.isEmpty) copy(left = Some(expr))
    else copy(right = Some(expr))

    def copy(left: Option[Expression] = left, right: Option[Expression] = right): Operation
}

case class Plus(override val left: Option[Expression] = None, override val right: Option[Expression] = None) extends Operation {
  override val op: Op = (_ + _)
  override def copy(left: Option[Expression] = left, right: Option[Expression] = right): Plus =
    Plus(left, right)
}
object Plus {
  def unapply(str: String): Option[Plus] =
    Some(str) filter (_ == "plus") map (_ => Plus())
}

case class Minus(override val left: Option[Expression] = None, override val right: Option[Expression] = None) extends Operation {
  override val op: Op = (_ - _)
  override def copy(left: Option[Expression] = left, right: Option[Expression] = right): Minus =
    Minus(left, right)
}
object Minus {
  def unapply(str: String): Option[Minus] =
    Some(str) filter (_ == "minus") map (_ => Minus())
}

case class Multiplied(override val left: Option[Expression] = None, override val right: Option[Expression] = None) extends Operation {
  override val op: Op = (_ * _)
  override def copy(left: Option[Expression] = left, right: Option[Expression] = right): Multiplied =
    Multiplied(left, right)
}
object Multiplied {
  def unapply(str: String): Option[Multiplied] =
    Some(str) filter (_ == "multiplied") map (_ => Multiplied())
}

case class Divided(override val left: Option[Expression] = None, override val right: Option[Expression] = None) extends Operation {
  override val op: Op = (_ / _)
  override def copy(left: Option[Expression] = left, right: Option[Expression] = right): Divided =
    Divided(left, right)
}
object Divided {
  def unapply(str: String): Option[Divided] =
    Some(str) filter (_ == "divided") map (_ => Divided())
}

case class Raised(override val left: Option[Expression] = None, override val right: Option[Expression] = None) extends Operation {
  override val op: Op = math.pow(_, _).toInt
  override def copy(left: Option[Expression] = left, right: Option[Expression] = right): Raised =
    Raised(left, right)
}
object Raised {
  def unapply(str: String): Option[Raised] =
    Some(str) filter (_ == "raised") map (_ => Raised())
}
