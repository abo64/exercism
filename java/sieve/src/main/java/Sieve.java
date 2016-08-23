import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Predicate;
import java.util.stream.IntStream;

public class Sieve {

    private final int n;

    public Sieve(int n) {
        this.n = n;
    }

    public List<Integer> getPrimes() {
        return IntStream.rangeClosed(2, n)
                .boxed()
                .reduce(new ArrayList<Integer>(), addIfPrime, first());
    }

    private static <T> BinaryOperator<T> first() {
        return (_1, _2) -> _1;
    }

    private static BiFunction<List<Integer>, Integer, List<Integer>> addIfPrime =
        (primes, candidate) -> {
            Predicate<Integer> squareLE = i -> i*i <= candidate;
            Predicate<Integer> notDivisibleBy = i -> candidate % i > 0;

            boolean candidateIsPrime =
                primes.stream()
                  .filter(squareLE)
                  .allMatch(notDivisibleBy);

            if (candidateIsPrime) primes.add(candidate);

            return primes;
        };
}