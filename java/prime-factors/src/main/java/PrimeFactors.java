import java.util.ArrayList;
import java.util.List;

public class PrimeFactors {

    public static List<Long> getForNumber(long number) {
//        return primeFactors(number, 2, new ArrayList<>());

        long dividend = number;
        long divisor = 2;
        List<Long> factors = new ArrayList<>();

        while (dividend > 1) {
            if (goesCleanlyInto(divisor, dividend)) {
                factors.add(divisor);
                dividend = dividend / divisor;
            } else {
                divisor = divisor + 1;
            }
        }

        return factors;
    }

    private static boolean goesCleanlyInto(long divisor, long dividend) {
        return dividend % divisor == 0;
    }

    // unfortunately Java doesn't have tail-recursion optimization -> StackOverflow!
//    private static List<Long> primeFactors(long dividend, long divisor, List<Long> factors) {
//        if (dividend == 1) {
//            return factors;
//        } else if (goesCleanlyInto(divisor, dividend)) {
//            factors.add(divisor);
//            return primeFactors(dividend / divisor, divisor, factors);
//        } else {
//            return primeFactors(dividend, divisor + 1, factors);
//        }
//    }
}
