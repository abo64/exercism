import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
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
                .collect(ArrayList::new, addIfPrime, List::addAll);
    }

    private static final BiConsumer<List<Integer>, Integer> addIfPrime =
        (primes, candidate) -> {
            Predicate<Integer> squareLE = i -> i * i <= candidate;
            Predicate<Integer> notDivisibleBy = i -> candidate % i > 0;

            boolean candidateIsPrime = primes.stream().filter(squareLE).allMatch(notDivisibleBy);

            if (candidateIsPrime)
                primes.add(candidate);
    };
}