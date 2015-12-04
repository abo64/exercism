import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class Raindrops {

    public static String convert(int number) {
        return rainDrops()
                 .map(primeFactorToRaindrop(number))
                 .filter(Optional::isPresent)
                 .map(Optional::get)
                 .reduce(String::concat)
                 .orElse(Integer.toString(number));
    }

    private static Function<Pair<Integer,String>,Optional<String>> primeFactorToRaindrop(int number) {
        Predicate<Pair<Integer, String>> isRaindrop =
            raindrop -> hasPrimeFactor(number, raindrop.first);
        return
            raindrop -> Optional.of(raindrop).filter(isRaindrop).map(rd -> rd.second);
    }

    public static boolean hasPrimeFactor(int number, int primeFactor) {
        return number % primeFactor == 0;
    }

    private static Stream<Pair<Integer,String>> rainDrops() {
        return Stream.of(
                new Pair<Integer,String>(3, "Pling"),
                new Pair<Integer,String>(5, "Plang"),
                new Pair<Integer,String>(7, "Plong"));
    }

    private static class Pair<F,S> {
        private F first;
        private S second;

        private Pair(F first, S second) {
            this.first = first;
            this.second = second;
        }
    }
}
