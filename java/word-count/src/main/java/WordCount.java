import static java.util.function.Function.identity;
import static java.util.stream.Collectors.groupingBy;

import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class WordCount {

    private static final Pattern SplitWords = Pattern.compile("\\W+");

    public Map<String, Integer> phrase(String phrase) {
        return SplitWords.splitAsStream(phrase.toLowerCase())
                .collect(groupingBy(identity(),
                         Collectors.collectingAndThen(Collectors.counting(), Long::intValue)));
    }
}
