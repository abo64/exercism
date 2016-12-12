import Bearing._
import Instruction._
import typeclass._

case class Robot(bearing: Bearing, coordinates: Coordinates) {
  def advance: Robot =
    copy(coordinates = bearing.advance(coordinates))

  // bring it into scope
  import CircularBearing._

  def turnRight(): Robot =
    copy(bearing = bearing.next)

  def turnLeft(): Robot =
    copy(bearing = bearing.previous)

  def simulate(instructions: Instructions): Robot = {
    def execute(robot: Robot, instruction: Instruction): Robot =
      instruction.execute(robot)

    instructions.foldLeft(this)(execute)
  }
}

object Bearing {
  type Coordinates = (Int,Int)

  private type Advance = Coordinates => Coordinates

  sealed trait Bearing {
    def advance: Advance
  }

  case object North extends Bearing {
    override val advance: Advance = { case (x, y) => (x, y + 1) }
  }
  case object South extends Bearing {
    override val advance: Advance = { case (x, y) => (x, y - 1) }
  }
  case object West extends Bearing {
    override val advance: Advance = { case (x, y) => (x - 1, y) }
  }
  case object East extends Bearing {
    override val advance: Advance = { case (x, y) => (x + 1, y) }
  }

  private val values: Seq[Bearing] = Seq(North, East, South, West)

  implicit object BoundedBearing extends Bounded[Bearing] {
    override val minBound = values.head
    override val maxBound = values.last
  }

  implicit object EnumBearing extends Enum[Bearing] {
    override val pred: Bearing => Bearing =
      bearing => values(values.indexOf(bearing) - 1)
    override val succ: Bearing => Bearing =
      bearing => values(values.indexOf(bearing) + 1)
  }

  implicit object CircularBearing extends Circular[Bearing] {
    override val bounded = BoundedBearing
    override val enum = EnumBearing
  }
}

object Instruction {
  type Instruction = Char
  type Instructions = Seq[Instruction]

  implicit class InstructionOps(self: Instruction) {
    def execute(robot: Robot): Robot =
      self match {
        case 'R' => robot.turnRight
        case 'L' => robot.turnLeft
        case 'A' => robot.advance
      }
  }
}
