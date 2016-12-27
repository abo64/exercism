import java.util.Arrays;
import java.util.function.IntPredicate;
import java.util.function.Supplier;
import java.util.stream.IntStream;

public class SumOfMultiples {

    public static int sum(int limit, int[] factors) {
        Supplier<IntStream> factorStream =
            () -> Arrays.stream(factors);

        IntPredicate isMultiple =
            x -> factorStream.get()
                   .anyMatch(factor -> x % factor == 0);

        return IntStream.range(1, limit)
                 .filter(isMultiple)
                 .sum();
    }
}
