import java.util.Arrays;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Acronym {

    private Acronym() {}

    public static String generate(String phrase) {
        Stream<String> words = splitWords(phrase);
        Stream<Character> acronymLetters =
            words
              .map(firstChar)
              .map(Character::toUpperCase);
        return mkString(acronymLetters);
    }

    private static final String WordSplitRegex = "(\\W+|\\p{Lower}(?=\\p{Upper}))";

    private static Stream<String> splitWords(String phrase) {
        return Arrays.stream(phrase.split(WordSplitRegex));
    }

    private static final Function<String, Character> firstChar =
        s -> s.charAt(0);

    private static <A> String mkString(Stream<A> as) {
        return as.map(Object::toString)
                 .collect(Collectors.joining());
    }
}
