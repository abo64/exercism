sealed trait Bowling {
  def roll(pins: Int): Bowling

  def score(): Either[Error, Int]
}
object Bowling {
  def apply(): Bowling = BowlingStart
  private val BowlingStart = BowlingScore(List(Frame(List(), false)))

}

case class BowlingScore(frames: List[Frame]) extends Bowling {
  override def roll(pins: Int): Bowling = {
    def addFillBall(f: Frame, fs: List[Frame]): Bowling = {
      val List(firstRoll, secondRoll) = f.rolls
      if (firstRoll == 10 && secondRoll != 10 && secondRoll + pins > 10)
        BowlingError(s"invalid roll: $pins")
      else
        BowlingScore(f.addRoll(pins)::fs)
    }

    val addToFinalFrame: PartialFunction[List[Frame], Bowling] = {
      case (f@Frame(ts, true))::fs if ts.length < 2 =>
        BowlingScore(f.addRoll(pins)::fs)
      case (f@Frame(ts, true))::fs if f.throws == 2 && (ts.head == 10 || f.spare) =>
        addFillBall(f, fs)
      case Frame(ts, true)::_ =>
        BowlingError("no more rolls possible")
    }

    val addToNormalFrame: PartialFunction[List[Frame], Bowling] = {
      case f::_ if f.complete =>
        BowlingScore(Frame(List(pins), frames.length == 9)::frames)
      case f::fs if f.pins + pins <= 10 =>
        BowlingScore(f.addRoll(pins)::fs)
    }

    val invalidRoll: PartialFunction[List[Frame], Bowling] = {
      case _ => BowlingError(s"invalid roll: $pins")
    }

    if (pins < 0 || pins > 10)
      BowlingError(s"invalid roll: $pins")
    else
      (addToFinalFrame orElse addToNormalFrame orElse invalidRoll)(frames)
  }

  override def score(): Either[Error, Int] = {
    if (frames.length < 10 || frames.exists(!_.complete)) Left(Error("incomplete game"))
    else {
      val totalScore = frames.reverse.tails.take(10).foldLeft(0) { (score, frames) =>
          score + frames.head.score(frames.tail)
      }

      Right(totalScore)
    }
  }
}

case class BowlingError(error: Error) extends Bowling {
  override def roll(pins: Int): Bowling = this
  override def score(): Left[Error, Int] = Left(error)
}

case class Error(errorText: String)
object Error {
  implicit def stringToError(errorText: String): Error = Error(errorText)
}

case class Frame(rolls: List[Int], finalFrame: Boolean) {
  def addRoll(roll: Int): Frame = Frame(rolls :+ roll, finalFrame)
  lazy val throws = rolls.length
  lazy val pins = rolls.sum
  def strike: Boolean = throws == 1 && pins == 10
  def spare:  Boolean = throws == 2 && pins == 10

  def complete: Boolean =
    if (!finalFrame) strike || throws == 2
    else {
      if (rolls.head == 10 || rolls.take(2).sum == 10) throws == 3
      else throws == 2
    }

  def score(nextFrames: List[Frame]): Int = {
    val bonus =
      if (finalFrame) 0
      else {
        lazy val nextRoll = nextFrames(0).rolls.head
        lazy val nextToNextRoll =
          nextFrames(0).rolls.tail.headOption getOrElse
            nextFrames(1).rolls.head

        if (strike) nextRoll + nextToNextRoll
        else if (spare) nextRoll
        else 0
      }

    pins + bonus
  }
}
