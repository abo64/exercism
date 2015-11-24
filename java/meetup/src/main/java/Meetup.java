import java.util.function.Function;
import java.util.function.Predicate;

import org.joda.time.DateTime;

public class Meetup {

    private final int month;
    private int year;

    public Meetup(int month, int year) {
        this.month = month;
        this.year = year;
    }

    public DateTime day(int weekDay, MeetupSchedule schedule) {
        switch (schedule) {
            case TEENTH:
                return next(weekDay, new DateTime(year, month, 12, 0, 0));
            case LAST:
                return previous(weekDay, new DateTime(year, month, 1, 0, 0).plusMonths(1));
            case FIRST:
                return next(weekDay, new DateTime(year, month, 1, 0, 0).minusDays(1));
            case SECOND:
                return day(weekDay, MeetupSchedule.FIRST).plusDays(7);
            case THIRD:
                return day(weekDay, MeetupSchedule.FIRST).plusDays(14);
            case FOURTH:
                return day(weekDay, MeetupSchedule.FIRST).plusDays(21);
        }
        return null;
    }

    private DateTime next(final int weekDay, DateTime startDate) {
        return loopUntil(startDate.plusDays(1),
                dt -> dt.getDayOfWeek() == weekDay, dt -> dt.plusDays(1));
//        return loopUntil(startDate.plusDays(1), weekDay, +1);
    }

    private DateTime previous(final int weekDay, DateTime startDate) {
        return loopUntil(startDate.minusDays(1),
                dt -> dt.getDayOfWeek() == weekDay, dt -> dt.minusDays(1));
//        return loopUntil(startDate.minusDays(1), weekDay, -1);
    }

    // yes, it's more complicated, but I wanted to apply Java 8 FP
    // (and this way loopUntil is also more general)
    private DateTime loopUntil(DateTime dateTime,
                Predicate<DateTime> found, Function<DateTime, DateTime> next) {
        if (found.test(dateTime)) return dateTime;
        else return loopUntil(next.apply(dateTime), found, next);
    }
//    private DateTime loopUntil(DateTime dateTime, int weekDay, int incrementDay) {
//        if (dateTime.getDayOfWeek() == weekDay) return dateTime;
//        else return loopUntil(dateTime.plusDays(incrementDay), weekDay, incrementDay);
//    }
}
