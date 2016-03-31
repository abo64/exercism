import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class Bob {

    public String hey(String phrase) {
        return phraseClassifiers()
                 .map(classifier -> classifier.classify(phrase))
                 .filter(response -> response.isPresent())
                 .map(response -> response.get())
                 .findFirst()
                 .orElse("Whatever.");
    }

    private interface PhraseClassifier extends Function<String, Optional<String>> {
        default Optional<String> classify(String phrase) {
            return apply(phrase);
        }
    }

    private static Stream<PhraseClassifier> phraseClassifiers() {
        return Stream.of(question, silence, shouting);
    }

    private static PhraseClassifier question =
        phrase ->
            Optional.of(phrase).filter(asPredicate("^.+\\?$"))
                 .filter(exists(Character::isLowerCase).or(exists(Character::isDigit)))
                 .map(ignore -> "Sure.");


    private static PhraseClassifier silence =
        phrase ->
            response("^\\s*$", phrase, "Fine. Be that way!");

    private static PhraseClassifier shouting =
        phrase ->
            Optional.of(phrase)
                 .filter(forall(c -> !Character.isLowerCase(c)))
                 .filter(exists(Character::isLetter))
                 .map(ignore -> "Whoa, chill out!");


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
