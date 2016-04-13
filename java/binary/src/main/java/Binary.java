import java.util.Optional;
import java.util.function.Function;
import java.util.function.IntBinaryOperator;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class Binary {

    private final String input;

    public Binary(String input) {
        this.input = input;}

    public int getDecimal() {
        return parse(input).orElse(0);
    }

    private static Optional<Integer> parse(String binary) {
        return Optional.ofNullable(binary).filter(isValidBinary).map(toDecimal);
    }

    private static IntBinaryOperator nextDigit =
        (output, bin) -> output * 2 + Character.digit(bin, 10);

    private static Function<String,Integer> toDecimal =
        binary -> binary.chars().reduce(0, nextDigit);

    private static Predicate<String> isValidBinary =
        candidate ->
            !candidate.isEmpty() &&
            characterStream(candidate).allMatch(c -> c == '0' || c == '1');

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
