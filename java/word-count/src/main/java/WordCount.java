import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class WordCount {

    public Map<String, Integer> Phrase(String phrase) {
        Map<String, List<String>> wordsByWord =
            wordStream(phrase)
                .map(word -> word.toLowerCase())
                .collect(Collectors.groupingBy(Function.identity()));
        return mapValues(wordsByWord, words -> words.size());
    }

	private Stream<String> wordStream(String phrase) {
        String[] words = phrase.split("\\W+");
        return Stream.of(words);
    }

    private static final <K,V, U> Map<K,U> mapValues(Map<K,V> source, Function<V, U> f) {
        return
          source.entrySet().stream()
            .map(e -> new Pair<K,U>(e.getKey(), f.apply(e.getValue())))
            .collect(pairsToMap());
    }

    private static final <K,V> Collector<Pair<K,V>, ?, Map<K,V>> pairsToMap() {
        return Collectors.toMap(Pair::getFirst, Pair::getSecond);
    }

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
