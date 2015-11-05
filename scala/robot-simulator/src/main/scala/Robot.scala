import Robot._
import Bearing._
import Instruction._

case class Robot(bearing: Bearing, coordinates: Coordinates) {
  def advance: Movement =
    copy(coordinates = bearing.advance(coordinates))

  def turnRight(): Movement =
    copy(bearing = bearing.right)

  def turnLeft(): Movement =
    copy(bearing = bearing.left)

  def simulate(instructions: Instructions): Movement = {
    def execute(robot: Robot, instruction: Instruction): Movement =
      instruction.execute(robot)

    instructions.foldLeft(this)(execute)
  }
}

object Robot {
  type Coordinates = (Int,Int)
  type Movement = Robot
}

object Bearing {
  sealed trait Bearing {
    def left: Bearing
    def right: Bearing
    def advancement: (Int,Int)

    def advance(coordinates: Coordinates): Coordinates =
      (coordinates._1 + advancement._1, coordinates._2 + advancement._2)
  }

  case object North extends Bearing {
    override val left = West
    override val right = East
    override val advancement = (0,1)
  }
  case object South extends Bearing {
    override val left = East
    override val right = West
    override val advancement = (0,-1)
  }
  case object West extends Bearing {
    override val left = South
    override val right = North
    override val advancement = (-1,0)
  }
  case object East extends Bearing {
    override val left = North
    override val right = South
    override val advancement = (1,0)
  }
}

object Instruction {
  type Instruction = Char
  type Instructions = Seq[Instruction]

  implicit class InstructionOps(self: Instruction) {
    def execute(robot: Robot): Movement =
      self match {
        case 'R' => robot.turnRight
        case 'L' => robot.turnLeft
        case 'A' => robot.advance
      }
  }
}