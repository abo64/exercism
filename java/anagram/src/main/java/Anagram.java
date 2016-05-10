import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Anagram {

    private final String lowerCaseWord;
    private final String sortedLowerCaseWord;

    public Anagram(String word) {
        this.lowerCaseWord = word.toLowerCase();
        this.sortedLowerCaseWord = sortedCharsStr(lowerCaseWord);
    }

    public List<String> match(List<String> candidates) {
        return candidates.stream()
                   .filter(sameCharsButNotIdentical)
                   .collect(Collectors.toList());
    }

    private Predicate<String> sameCharsButNotIdentical =
        candidate -> {
            String lowerCaseCandidate = candidate.toLowerCase();
            return this.sortedLowerCaseWord.equals(sortedCharsStr(lowerCaseCandidate))
                    && !this.lowerCaseWord.equals(lowerCaseCandidate);
        };

    private static String sortedCharsStr(String str) {
        Stream<Character> sortedChars =
            characterStream(str).sorted();
        return mkString(sortedChars);
    }

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }

    private static <A> String mkString(Stream<A> as) {
        return as.map(Object::toString)
                 .collect(Collectors.joining(""));
    }
}
