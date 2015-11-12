import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Etl {

    public Map<String, Integer> transform(Map<Integer, List<String>> oldScores) {
        Stream<Pair<String, Integer>> newScores =
            oldScores.entrySet().stream().flatMap(oldScoreToNewScores);
        return newScores.collect(pairsToMap());
    }

    private static final Function<
      Entry<Integer, List<String>>,
      Stream<Pair<String, Integer>>> oldScoreToNewScores =
        oldScore -> {
            Integer point = oldScore.getKey();
            List<String> letters = oldScore.getValue();
            Stream<Pair<String, Integer>> newScores =
                letters.stream().map(l -> new Pair<String, Integer>(l.toLowerCase(), point));
            return newScores;
        };

    private static final <K,V> Collector<Pair<K,V>, ?, Map<K,V>> pairsToMap() {
        return Collectors.toMap(Pair::getFirst, Pair::getSecond);
    }

    // will there ever be built-in Tuples in Java?
    private static class Pair<F,S> {
        private F first;
        private S second;

        private Pair(F first, S second) {
            this.first = first;
            this.second = second;
        }

        public F getFirst() { return first; }
        public S getSecond() { return second; }
    }
}
