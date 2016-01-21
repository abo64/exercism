import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

public class Prime {

    public static int nth(int n) {
        if (n < 1) throw new IllegalArgumentException("must be > 0: " + n);
        return sieve(n);
//        return sieve(n, 2, new ArrayList<>());
    }

    // // Sieve of Eratosthenes
    private static int sieve(int n) {
        List<Integer> primes = new ArrayList<>();
        int candidate = 2;

        do {
            boolean candidateIsPrime =
               primes.stream()
                .filter(squareLE(candidate))
                .allMatch(notDivisibleBy(candidate));

            if (candidateIsPrime) primes.add(candidate);
            candidate = candidate + 1;
        } while (primes.size() < n);

        return primes.get(primes.size() - 1);
    }

    private static Predicate<Integer> squareLE(int candidate) {
        return i -> i*i <= candidate;
    }

    private static Predicate<Integer> notDivisibleBy(int candidate) {
        return i -> candidate % i != 0;
    }

    // sorry, but Java has no tail-recursion optimization
//    private static int sieve(int n, int candidate, List<Integer> primes) {
//        int primesSize = primes.size();
//        if (primesSize == n) return primes.get(primesSize - 1);
//        else {
//            Predicate<Integer> squareLE = i -> i*i <= candidate;
//            Predicate<Integer> notDivisibleBy = i -> candidate % i != 0;
//
//            boolean candidateIsPrime =
//               primes.stream()
//                .filter(squareLE)
//                .allMatch(notDivisibleBy);
//
//            if (candidateIsPrime) primes.add(candidate);
//            int nextCandidate = candidate + 1;
//            return sieve(n, nextCandidate, primes);
//        }
//    }
}
