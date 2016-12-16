import java.util.stream.IntStream;

public final class Difference {
    public static int computeSumOfSquaresTo(int n) {
        return IntStream.rangeClosed(1, n)
                 .map(Difference::square)
                 .sum();
    }

    public static int computeSquareOfSumTo(int n) {
        int sumToN = IntStream.rangeClosed(1, n).sum();
        return square(sumToN);
    }

    public static int betweenSquareOfSumAndSumOfSquaresTo(int n) {
        return computeSquareOfSumTo(n) - computeSumOfSquaresTo(n);
    }

    private static int square(int n) {
        return n * n;
    }
}
