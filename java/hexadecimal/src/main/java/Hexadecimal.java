import java.util.Optional;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Stream;

public class Hexadecimal {

    public static int toDecimal(String hex) {
        Stream<Character> hexDigits = characterStream(hex.toUpperCase());

        return hexDigits
                 .map(hexToDecDigit)
                 .reduce(Zero, accumulateDecimal)
                 .orElse(0);
    }

    // some kind of Scala's for-comprehension of Haskell's monadic do would be nice here
    private static final BinaryOperator<Optional<Integer>> accumulateDecimal =
        (maybeDecimal, maybeDecDigit) ->
            maybeDecimal.flatMap(decimal ->
                maybeDecDigit.map(decDigit ->
                    decimal * 16 + decDigit));

    private static final Optional<Integer> Zero = Optional.of(0);

    private static final String HexDigits = "0123456789ABCDEF";

    private static final Function<Character,Optional<Integer>> hexToDecDigit =
        c -> Optional.of(HexDigits.indexOf(c)).filter(i -> i >= 0);

    private static Stream<Character> characterStream(CharSequence str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
