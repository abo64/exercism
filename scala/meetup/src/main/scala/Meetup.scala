import java.time.LocalDate
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
    val values: Seq[WeekDay] = Seq(Mon, Tue, Wed, Thu, Fri, Sat, Sun)

    implicit def weekDayToCalendarDayOfWeek(weekDay: WeekDay): CalendarDayOfWeek =
      values.indexOf(weekDay) + 1

    implicit def calendarDayOfWeekToWeekDay(dayOfWeek: CalendarDayOfWeek): WeekDay =
      values(dayOfWeek - 1)
  }

  type MeetupDate = LocalDate
  object MeetupDate {
    def apply(year: Int = 1970, month: Int = 1, dayOfMonth: Int = 1): MeetupDate =
      LocalDate.of(year, month, dayOfMonth)
  }

  implicit class MeetupDateOps(self: MeetupDate) {

    def year = self.getYear
    def month = self.getMonth.getValue
    def dayOfMonth = self.getDayOfMonth
    def weekDay: WeekDay = self.getDayOfWeek.getValue

    def copy(year: Int = self.year, month: Int = self.month, dayOfMonth: Int = self.dayOfMonth) =
      MeetupDate(year, month, dayOfMonth)

    def loopUntil(terminate: MeetupDate => Boolean, next: MeetupDate => MeetupDate): MeetupDate = {
      @tailrec def loop(md: MeetupDate): MeetupDate =
        if (terminate(md)) md else loop(next(md))

      loop(self)
    }

    def next(weekDay: WeekDay): MeetupDate =
      nextDay loopUntil (_.weekDay == weekDay, _.nextDay)

    def previous(weekDay: WeekDay): MeetupDate =
      previousDay loopUntil (_.weekDay == weekDay, _.previousDay)

    def nextDay: MeetupDate = self.plusDays(1)
    def previousDay: MeetupDate = self.minusDays(1)

    def nextWeek: MeetupDate = self.plusWeeks(1)
    def previousWeek: MeetupDate = self.minusWeeks(1)

    def nextMonth: MeetupDate =
      self.copy(dayOfMonth = 1).plusMonths(1)
  }
}
