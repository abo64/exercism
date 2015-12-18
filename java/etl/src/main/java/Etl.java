import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiFunction;

public class Etl {

    public Map<String, Integer> transform(Map<Integer, List<String>> oldScores) {
        return foldLeft(oldScores.entrySet(), new HashMap<>(), oldScoreToNewScores);
    }

    private static final BiFunction<Map<String, Integer>, Entry<Integer, List<String>>,
            Map<String, Integer>> oldScoreToNewScores =
        (newScores, oldScore) -> {
            Integer point = oldScore.getKey();
            List<String> letters = oldScore.getValue();
            // sorry for the side effect - Java 8 FP requires compromises! ;-)
            letters.stream().forEach(l -> newScores.put(l.toLowerCase(), point));
            return newScores;
        };

    private static <A, B> B foldLeft(Iterable<A> as, B z, BiFunction<B, A, B> f) {
        B result = z;
        for (A a : as) {
            result = f.apply(result, a);
        }
        return result;
    }
}
