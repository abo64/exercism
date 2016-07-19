import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Etl {

    public Map<String, Integer> transform(Map<Integer, List<String>> oldScores) {
        return oldScores.entrySet().stream()
                .collect(Collectors.reducing(new HashMap<String, Integer>(),
                         oldScoreToNewScores, mergeMaps));
    }

    private static final Function<Entry<Integer, List<String>>,
        Map<String, Integer>> oldScoreToNewScores =
            (oldScore) -> {
              Function<String,Integer> point = constant(oldScore.getKey());
              List<String> letters = oldScore.getValue();
              return letters.stream()
                       .map(String::toLowerCase)
                       .collect(Collectors.toMap(Function.identity(), point));
            };

    private static final BinaryOperator<Map<String, Integer>> mergeMaps =
        (map1, map2) -> {
            map1.putAll(map2);
            return map1;
        };

    private static final <T,U> Function<T,U> constant(U u) {
        return (ignored) -> u;
    }
}
