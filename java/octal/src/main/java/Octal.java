import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Octal {

    private final String octal;

    public Octal(String octal) {
        this.octal = octal;
    }

    public int getDecimal() {
        if (!isValidOctal(octal)) return 0;

        List<Character> octalDigits =
            characterStream(octal).collect(Collectors.toList());

        return foldLeft(octalDigits, Integer.valueOf(0), octalToDecimal);
    }

    private static final BiFunction<Integer, Character, Integer> octalToDecimal =
        (decimal, octalDigit) -> decimal * 8 + Character.getNumericValue(octalDigit);

    private static boolean isValidOctal(String octal) {
        Predicate<Character> isOctalDigit =
            c -> "01234567".indexOf(c) > -1;

        return (!octal.isEmpty()) &&
                characterStream(octal).allMatch(isOctalDigit);
    }

    private static <A, B> B foldLeft(Iterable<A> as, B z, BiFunction<B,A,B> f) {
        B result = z;
        for (A a : as) {
            result = f.apply(result, a);
        }
        return result;
    }

    private static Stream<Character> characterStream(CharSequence str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
