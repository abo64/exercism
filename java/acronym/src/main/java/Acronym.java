import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Acronym {

    private Acronym() {}

    public static String generate(String phrase) {
        Stream<Character> acronymLetters =
            SplitWords.splitAsStream(phrase)
              .map(firstChar)
              .map(Character::toUpperCase);
        return mkString(acronymLetters);
    }

    private static final Pattern SplitWords =
            Pattern.compile("(\\W+|\\p{Lower}(?=\\p{Upper}))");

    private static final Function<String, Character> firstChar =
        s -> s.charAt(0);

    private static <A> String mkString(Stream<A> as) {
        return as.map(Object::toString)
                 .collect(Collectors.joining());
    }
}
