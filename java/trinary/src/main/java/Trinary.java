import java.util.function.Predicate;
import java.util.stream.Stream;

public class Trinary {

    public static int toDecimal(String trinary) {
        Stream<Character> trinaryDigits = characterStream(trinary);
        if (trinary.isEmpty() || !trinaryDigits.allMatch((isTrinaryDigit))) return 0;
        else return trinaryToInt(trinary);
    }

    private static int trinaryToInt(String trinary) {
        return characterStream(trinary)
          .map(Character::getNumericValue)
          .reduce(0, (acc, trinaryDigit) -> acc * 3 + trinaryDigit);
    }

    private static final Predicate<Character> isTrinaryDigit =
        c -> "012".indexOf(c) != -1;

    private static Stream<Character> characterStream(CharSequence str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
