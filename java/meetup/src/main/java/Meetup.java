import java.util.Iterator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

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

    private static DateTime next(final int weekDay, DateTime startDate) {
        return loopUntil(startDate.plusDays(1),
                dt -> dt.getDayOfWeek() == weekDay, dt -> dt.plusDays(1));
    }

    private static DateTime previous(final int weekDay, DateTime startDate) {
        return loopUntil(startDate.minusDays(1),
                dt -> dt.getDayOfWeek() == weekDay, dt -> dt.minusDays(1));
    }

    private static DateTime loopUntil(DateTime dateTime,
                Predicate<DateTime> found, Function<DateTime, DateTime> next) {
        return dateTimeStream(dateTime, next)
                 .filter(found)
                 .findFirst()
                 .get();
    }

    private static Stream<DateTime> dateTimeStream(final DateTime startDateTime,
                final Function<DateTime, DateTime> next) {
        Iterator<DateTime> dateTimeIter =
            new Iterator<DateTime>() {
                private DateTime currentDateTime = startDateTime;

                @Override public boolean hasNext() { return true; }

                @Override public DateTime next() {
                    DateTime current = currentDateTime;
                    currentDateTime = next.apply(current);
                    return current;
                }
            };

            return fromIterator(dateTimeIter)
                     .limit(31); // emergency break
    }

    private static <T> Stream<T> fromIterator(Iterator<T> iterator) {
        Iterable<T> iterable = () -> iterator;
        return StreamSupport.stream(iterable.spliterator(), false);
    }
}
