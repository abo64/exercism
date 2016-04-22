import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

public class PrimeFactors {

    public static List<Long> getForNumber(long number) {
        List<Long> primeNumbers = unfoldRight(new Pair<>(2L, number), nextPrimeFactor);
        return filterSkips(primeNumbers);
    }

    private static final Long SkipMe = 0L;

    private static final Function<Pair<Long, Long>, Optional<Pair<Long, Pair<Long, Long>>>> nextPrimeFactor =
            pair -> {
                Long divisor = pair.first;
                Long dividend = pair.second;
                if (dividend <= 1) return Optional.empty();
                else if (goesCleanlyInto(divisor, dividend))
                     return Optional.of(new Pair<>(divisor, new Pair<>(divisor, dividend / divisor)));
                else return Optional.of(new Pair<>(SkipMe,  new Pair<>(divisor + 1, dividend)));
            };

    private static boolean goesCleanlyInto(long divisor, long dividend) {
        return dividend % divisor == 0;
    }

    private static List<Long> filterSkips(List<Long> candidates) {
        return candidates.stream().filter(n -> n != SkipMe).collect(Collectors.toList());
    }

    private static <A, B> List<A> unfoldRight(B seed, Function<B, Optional<Pair<A, B>>> f) {
        List<A> result = new ArrayList<>();
        Optional<Pair<A, B>> maybeNext = f.apply(seed);
        while (maybeNext.isPresent()) {
            Pair<A, B> next = maybeNext.get();
            result.add(next.first);
            maybeNext = f.apply(next.second);
        }
        return result;
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
