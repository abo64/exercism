import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Hexadecimal {

    public static int toDecimal(String hex) {
        if (!isValidHex(hex)) return 0;

        List<Character> hexDigits =
            characterStream(hex.toUpperCase()).collect(Collectors.toList());

        return foldLeft(hexDigits, Integer.valueOf(0), hexToDecimal);
    }

    private static final BiFunction<Integer, Character, Integer> hexToDecimal =
        (decimal, hexDigit) -> decimal * 16 + Character.getNumericValue(hexDigit);

    private static boolean isValidHex(String hex) {
        Predicate<Character> isHexDigit =
            c -> "0123456789ABCDEF".indexOf(c) > -1;

        return (!hex.isEmpty()) &&
                characterStream(hex.toUpperCase()).allMatch(isHexDigit);
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
