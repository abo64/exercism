import java.util.Arrays;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class Raindrops {

    public static String convert(int number) {
        Predicate<Raindrop> keepRaindrop =
            raindrop -> raindrop.hasPrimeFactor(number);

        return rainDrops()
                 .filter(keepRaindrop)
                 .map(Raindrop::toString)
                 .reduce(String::concat)
                 .orElse(Integer.toString(number));
    }

    private static Stream<Raindrop> rainDrops() {
        return Arrays.stream(Raindrop.values());
    }

    private enum Raindrop {
        Pling(3),
        Plang(5),
        Plong(7);

        private final int primeFactor;

        private Raindrop(int primeFactor) {
            this.primeFactor = primeFactor;
        }

        private boolean hasPrimeFactor(int number) {
            return number % primeFactor == 0;
        }
    }
}
