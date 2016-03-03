import static java.util.function.Function.identity;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.summingInt;

import java.util.Map;
import java.util.regex.Pattern;

public class WordCount {

    private static final Pattern SplitWords = Pattern.compile("\\W+");

    public Map<String, Integer> phrase(String phrase) {
        return SplitWords.splitAsStream(phrase.toLowerCase())
                .collect(groupingBy(identity(), summingInt(e -> 1)));
    }
}
