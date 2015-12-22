import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Pangrams {

    public static boolean isPangram(String sentence) {
        String lowerCaseSentence = sentence.toLowerCase();

        Predicate<Character> isContainedInSentence =
            c -> lowerCaseSentence.indexOf(c) != -1;

        return alphabet().allMatch(isContainedInSentence);
    }

    private static Stream<Character> alphabet() {
        return IntStream.rangeClosed('a', 'z').mapToObj(i -> (char)i);
    }
}
