import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

public class Gigasecond {

    private LocalDateTime startDateTime;

    public Gigasecond(LocalDate startDate) {
        this.startDateTime = LocalDateTime.of(startDate, LocalTime.of(0, 0));
    }

    public Gigasecond(LocalDateTime startDateTime) {
        this.startDateTime = startDateTime;
    }

    private static final long GIGA_SECONDS = (long)Math.pow(10, 9);

    public LocalDateTime getDate() {
        return startDateTime.plusSeconds(GIGA_SECONDS);
    }
}
