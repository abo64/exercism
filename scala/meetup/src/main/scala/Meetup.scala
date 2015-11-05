import scala.annotation.tailrec
import Meetup._

class Meetup(month: Int, year: Int) {
  def teenth(weekDay: WeekDay): MeetupDate =
    MeetupDate(year, month, 12) next weekDay

  def first(weekDay: WeekDay): MeetupDate =
    MeetupDate(year, month, 1).previousDay next weekDay

  def second(weekDay: WeekDay): MeetupDate =
    first(weekDay) nextWeek

  def third(weekDay: WeekDay): MeetupDate =
    second(weekDay) nextWeek

  def fourth(weekDay: WeekDay): MeetupDate =
    third(weekDay) nextWeek

  def last(weekDay: WeekDay): MeetupDate =
    MeetupDate(year, month).nextMonth previous weekDay
}

object Meetup {
  def apply(month: Int, year: Int) = new Meetup(month: Int, year: Int)

  sealed trait WeekDay
  case object Mon extends WeekDay
  case object Tue extends WeekDay
  case object Wed extends WeekDay
  case object Thu extends WeekDay
  case object Fri extends WeekDay
  case object Sat extends WeekDay
  case object Sun extends WeekDay

  type CalendarDayOfWeek = Int

  object WeekDay {
    val values: Seq[WeekDay] = Seq(Sun, Mon, Tue, Wed, Thu, Fri, Sat)

//    def distance(from: WeekDay, to: WeekDay, forward: Boolean = true): Int = {
//      val (fromIndex, toIndex) = (values.indexOf(from), values.indexOf(to))
//      var dist = if (forward) toIndex - fromIndex else fromIndex - toIndex
//      if (dist == 0) dist = 7
//      val result = if (dist < 0) dist + 7 else dist
//      if (forward) result else -result
//    }

    implicit def weekDayToCalendarDayOfWeek(weekDay: WeekDay): CalendarDayOfWeek =
      values.indexOf(weekDay) + 1

    implicit def calendarDayOfWeekToWeekDay(dayOfWeek: CalendarDayOfWeek): WeekDay =
      values(dayOfWeek - 1)
  }

  import java.util.GregorianCalendar
  type MeetupDate = GregorianCalendar
  object MeetupDate {
    def apply(year: Int = 1970, month: Int = 1, dayOfMonth: Int = 1): MeetupDate =
      new GregorianCalendar(year, month - 1, dayOfMonth)
  }

  implicit class MeetupDateOps(self: MeetupDate) {
    import java.util.Calendar._

    def year = self.get(YEAR)
    def month = self.get(MONTH) + 1
    def dayOfMonth = self.get(DAY_OF_MONTH)
    def weekDay: WeekDay = self.get(DAY_OF_WEEK)

    def copy(year: Int = self.year, month: Int = self.month, dayOfMonth: Int = self.dayOfMonth) =
      MeetupDate(year, month, dayOfMonth)

    def loopUntil(terminate: MeetupDate => Boolean, next: MeetupDate => MeetupDate): MeetupDate = {
      @tailrec def loop(md: MeetupDate): MeetupDate =
        if (terminate(md)) md else loop(next(md))

      loop(self)
    }

    def next(weekDay: WeekDay): MeetupDate =
      nextDay loopUntil (_.weekDay == weekDay, _.setNextDay)
//      copy(dayOfMonth = dayOfMonth + WeekDay.distance(self.weekDay, weekDay))

    def previous(weekDay: WeekDay): MeetupDate =
      previousDay loopUntil (_.weekDay == weekDay, _.setPreviousDay)
//      copy(dayOfMonth = dayOfMonth + WeekDay.distance(self.weekDay, weekDay, false))

    def nextDay: MeetupDate = self.copy(dayOfMonth = dayOfMonth + 1)
    def previousDay: MeetupDate = self.copy(dayOfMonth = dayOfMonth - 1)

    def nextWeek: MeetupDate = self.copy(dayOfMonth = dayOfMonth + 7)
    def previousWeek: MeetupDate = self.copy(dayOfMonth = dayOfMonth - 7)

    // for efficiency: side effect on self instead of creating a copy
    private def effect(ef: MeetupDate => Unit): MeetupDate = { ef(self); self }
    def setNextDay: MeetupDate = effect(_.add(DAY_OF_MONTH, +1))
    def setPreviousDay: MeetupDate = effect(_.add(DAY_OF_MONTH, -1))

    def nextMonth: MeetupDate =
      self.copy(dayOfMonth = 1, month = month + 1)
  }
}
