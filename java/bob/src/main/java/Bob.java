import java.util.Optional;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class Bob {

    public String hey(String phrase) {
        return Stream.of(question(phrase), silence(phrase), shouting(phrase))
                 .filter(response -> response.isPresent())
                 .map(response -> response.get())
                 .findFirst()
                 .orElse("Whatever.");
    }

    private static Optional<String> question(String phrase) {
        return Optional.of(phrase).filter(asPredicate("^.+\\?$"))
                 .filter(exists(Character::isLowerCase).or(exists(Character::isDigit)))
                 .map(ignore -> "Sure.");
    }

    private static Optional<String> silence(String phrase) {
        return response("^\\s*$", phrase, "Fine. Be that way!");
    }

    private static Optional<String> shouting(String phrase) {
        return Optional.of(phrase)
                 .filter(forall(c -> !Character.isLowerCase(c)))
                 .filter(exists(Character::isLetter))
                 .map(ignore -> "Whoa, chill out!");
    }

    private static Predicate<String> forall(Predicate<Character> p) {
        return str -> characterStream(str).allMatch(p);
    }

    private static Predicate<String> exists(Predicate<Character> p) {
        return str -> characterStream(str).anyMatch(p);
    }

    private static Optional<String> response(String regex, String phrase, String reply) {
        return Optional.of(phrase).filter(asPredicate(regex)).map(ignore -> reply);
    }

    private static Predicate<String> asPredicate(String regex) {
        return Pattern.compile(regex).asPredicate();
    }

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
