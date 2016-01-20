import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;

public class Etl {

    public Map<String, Integer> transform(Map<Integer, List<String>> oldScores) {
        return oldScores.entrySet().stream()
                 .reduce(new HashMap<String, Integer>(), oldScoreToNewScores, putAll);
    }

    private static final BinaryOperator<Map<String, Integer>> putAll =
        (accMap, map) -> {
            accMap.putAll(map);
            return accMap;
        };

    private static final BiFunction<Map<String, Integer>, Entry<Integer, List<String>>,
            Map<String, Integer>> oldScoreToNewScores =
        (newScores, oldScore) -> {
            Integer point = oldScore.getKey();
            List<String> letters = oldScore.getValue();
            // sorry for the side effect - Java 8 FP requires compromises! ;-)
            letters.stream()
              .map(String::toLowerCase)
              .forEach(l -> newScores.put(l, point));
            return newScores;
        };
}
