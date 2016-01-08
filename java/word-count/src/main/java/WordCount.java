import static java.util.function.Function.identity;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.summingInt;

import java.util.Arrays;
import java.util.Map;

public class WordCount {

    public Map<String, Integer> phrase(String phrase) {
        String[] tokens = phrase.split("\\W+");
        return Arrays.stream(tokens)
                .map(String::toLowerCase)
                .collect(groupingBy(identity(), summingInt(e -> 1)));
    }
}
