import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Sieve {

    private final int n;

    public Sieve(int n) {
        this.n = n;
    }

    public List<Integer> getPrimes() {
        List<Integer> candidates =
            IntStream.rangeClosed(2, n)
             .mapToObj(Integer::new)
             .collect(Collectors.toList());

        return foldLeft(candidates, new ArrayList<Integer>(), addIfPrime);
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

    private static <A, B> B foldLeft(Iterable<A> as, B z, BiFunction<B,A,B> f) {
        B result = z;
        for (A a : as) {
            result = f.apply(result, a);
        }
        return result;
    }
}
