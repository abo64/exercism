import java.util.function.BinaryOperator;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class Octal {

    private final String octal;

    public Octal(String octal) {
        this.octal = octal;
    }

    public int getDecimal() {
        if (!isValidOctal(octal)) return 0;

        return characterStream(octal)
                 .map(Character::getNumericValue)
                 .reduce(0, octalToDecimal);
    }

    private static final BinaryOperator<Integer> octalToDecimal =
        (decimal, octalDigit) -> decimal * 8 + octalDigit;

    private static boolean isValidOctal(String octal) {
        Predicate<Character> isOctalDigit =
            c -> "01234567".indexOf(c) > -1;

        return (!octal.isEmpty()) &&
                characterStream(octal).allMatch(isOctalDigit);
    }

    private static Stream<Character> characterStream(CharSequence str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
