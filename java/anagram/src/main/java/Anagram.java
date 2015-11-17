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
        new Predicate<String>() {
            @Override public boolean test(String candidate) {
                String lowerCaseCandidate = candidate.toLowerCase();
                return sortedLowerCaseWord.equals(sortedCharsStr(lowerCaseCandidate)) &&
                       !lowerCaseWord.equals(lowerCaseCandidate);
            }};

    private static String sortedCharsStr(String str) {
        return characterStream(str)
                 .sorted()
                 .collect(StringBuilder::new,
                          (sb, i) -> sb.append((char)i),
                          StringBuilder::append)
                 .toString();
    }

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }

}
