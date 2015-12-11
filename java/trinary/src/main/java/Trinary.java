import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class Trinary {

    public static int toDecimal(String trinary) {
        Stream<Character> trinaryDigits = characterStream(trinary);
        if (trinary.isEmpty() || !trinaryDigits.allMatch((isTrinaryDigit))) return 0;
        else return trinaryToInt(trinary);
    }

    private static int trinaryToInt(String trinary) {
        Iterable<Character> trinaryDigits = characterStream(trinary)::iterator;
        return foldLeft(trinaryDigits, 0,
                 (acc, trinaryDigit) -> acc * 3 + Character.getNumericValue(trinaryDigit));
    }

    private static final Predicate<Character> isTrinaryDigit =
        c -> "012".indexOf(c) != -1;

    private static <A, B> B foldLeft(Iterable<A> as, B z, BiFunction<B, A, B> f) {
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
