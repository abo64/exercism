import Clock._

trait Clock {

  val hour: Hour
  val min: Min

  override def toString: String =
    f"$hour%02d:$min%02d"

  override def equals(any: Any): Boolean =
    any match {
      case otherClock: Clock =>
        hour == otherClock.hour && min == otherClock.min
      case _ => false
    }

  def +(other: Clock): Clock = {
    val (newHour, newMin) = addMinsAndHours(min, other.min, hour, other.hour)
    Clock(newHour, newMin)
  }
  def -(other: Clock): Clock =  {
    val (newHour, newMin) = addMinsAndHours(min, -other.min, hour, -other.hour)
    Clock(newHour, newMin)
  }
}

object Clock {
  type Hour = Int
  type Min = Int

  def apply(hours: Hour, mins: Min): Clock = {
    val (newHour, newMin) = addMinsAndHours(mins, 0, hours, 0)
    new Clock { override val hour = newHour ; override val min = newMin}
  }
  def apply(min: Min): Clock = Clock(0, min)

  private def addMinsAndHours(min1: Min, min2: Min, hour1: Hour, hour2: Hour): (Hour, Min) = {
    val (minHour, newMin) = addMins(min1, min2)
    val newHour = addHours(hour1, hour2, minHour)
    (newHour, newMin)
  }

  private def addMins(min1: Min, min2: Min): (Hour, Min) = {
    val sum = min1 + min2
    val hour = if (sum < 0) -1 else if (sum > 59) 1 else 0
    (hour, (sum + 60) % 60)
  }

  private def addHours(hour1: Hour, hour2: Hour, hour3: Hour): Hour = {
    (hour1 + hour2 + hour3 + 24) % 24
  }
}