import java.util.GregorianCalendar
import Gigasecond._

case class Gigasecond(start: Date) {
  lazy val date: Date = addSeconds(start, OneGigaSec)
}

object Gigasecond {
  type Date = GregorianCalendar
  type Seconds = Long
  type Milliseconds = Long

  val OneGigaSec: Seconds = math.pow(10, 9) toLong

  val MillisPerSecond = 1000L

  def addSeconds(date: Date, seconds: Seconds): Date = {
    val dateInMilliseconds: Milliseconds = date.getTime.getTime
    val timestamp: Milliseconds = dateInMilliseconds + seconds * MillisPerSecond

    val result = date.clone.asInstanceOf[Date]
    result.setTimeInMillis(timestamp)
    result
  }
}
