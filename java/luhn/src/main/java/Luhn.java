import java.util.Arrays;
import java.util.function.IntUnaryOperator;
import java.util.function.LongPredicate;
import java.util.stream.IntStream;
import java.util.stream.LongStream;

public class Luhn {

    private final long number;

    public Luhn(long number) {
        this.number = number;
    }

    public int getCheckDigit() {
        int[] addends = getAddends();
        return addends[addends.length - 1];
    }

    private static IntUnaryOperator luhnTransform(String digits) {
        int maxIndex = digits.length() - 1;
        return index -> {
            int digit = Character.getNumericValue(digits.charAt(index));
            boolean applyLuhnTransformation = (maxIndex - index) % 2 != 0;
            if (applyLuhnTransformation) {
                int doubledDigit = digit * 2;
                if (doubledDigit > 9) return doubledDigit - 9;
                else return doubledDigit;
            }
            else return digit;
        };
    }

    public int[] getAddends() {
        String digits = Long.toString(number);
        int maxIndex = digits.length()  - 1;

        return IntStream.rangeClosed(0, maxIndex)
                 .map(luhnTransform(digits))
                 .toArray();
    }

    public int getCheckSum() {
        return Arrays.stream(getAddends()).sum();
    }

    public boolean isValid() {
        return getCheckSum() % 10 == 0;
    }

    private static final LongPredicate isValidLuhn =
        number -> new Luhn(number).isValid();

    public static long create(long number) {
        LongStream candidates =
            LongStream.rangeClosed(0, 9)
              .map(checkDigit -> number*10 + checkDigit);

        return candidates
                 .filter(isValidLuhn)
                 .findFirst()
                 .getAsLong();
    }
}
