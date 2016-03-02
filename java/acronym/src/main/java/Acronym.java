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

    private static final String WordSplitRegex = "[ -]";
    // see http://stackoverflow.com/questions/7593969/regex-to-split-camelcase-or-titlecase-advanced
    private static final String CamelCaseSplitRegex =
        "(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])";

    private static Stream<String> splitWords(String phrase) {
        return splitBy(phrase, WordSplitRegex)
                 .flatMap(word -> splitBy(word, CamelCaseSplitRegex));
    }

    private static Stream<String> splitBy(String str, String regex) {
        String[] words = str.split(regex);
        return Arrays.stream(words);
    }

    private static final Function<String, Character> firstChar =
        s -> s.charAt(0);

    private static <A> String mkString(Stream<A> as) {
        return as.map(Object::toString)
                 .collect(Collectors.joining());
    }
}
