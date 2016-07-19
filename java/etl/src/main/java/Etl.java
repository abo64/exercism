import java.util.AbstractMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Etl {

    public Map<String, Integer> transform(Map<Integer, List<String>> oldScores) {
        return oldScores.entrySet().stream()
                .flatMap(oldScoreToNewScores)
                .collect(Collectors.toMap(Entry::getKey , Entry::getValue));
    }

    private static final Function<Entry<Integer, List<String>>,
        Stream<Entry<String, Integer>>> oldScoreToNewScores =
            oldScore -> {
                Integer point = oldScore.getKey();
                Function<String,Entry<String, Integer>> toNewScore =
                    letter -> new AbstractMap.SimpleEntry<>(letter, point);
                List<String> letters = oldScore.getValue();
                return letters.stream()
                         .map(String::toLowerCase)
                         .map(toNewScore);
            };
}
