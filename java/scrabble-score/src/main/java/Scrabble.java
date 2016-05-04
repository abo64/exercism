import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Scrabble {

    private final Optional<String> word;

    public Scrabble(String word) {
        this.word = Optional.ofNullable(word);
    }

    private static final Function<Character,Integer> charScore =
        createCharScore().compose(Character::toUpperCase);

    public int getScore() {
        return word.map(toScore).orElse(0);
    }

    private Function<String,Integer> toScore =
        word -> characterStream(word).collect(Collectors.summingInt(c -> charScore.apply(c)));
//        word -> characterStream(word).reduce(0, addScore, (i, j) -> i + j);
//        word -> characterStream(word).map(charScore).mapToInt(i -> i.intValue()).sum();

    private static Function<Character,Integer> createCharScore() {
        Stream<Pair<Character, Integer>> charScores =
          concatStreams(
            charScoreStream("AEIOULNRST", 1),
            charScoreStream("DG", 2),
            charScoreStream("BCMP", 3),
            charScoreStream("FHVWY", 4),
            charScoreStream("K", 5),
            charScoreStream("JX", 8),
            charScoreStream("QZ", 10));
        Map<Character, Integer> charScoreMap = charScores.collect(pairsToMap());
        return mapToFunction(charScoreMap, Optional.of(0));
    }

    private static <K,V> Function<K,V> mapToFunction(Map<K,V> map, Optional<V> defaultValue) {
        return key -> defaultValue.isPresent() ?
            map.getOrDefault(key, defaultValue.get()) : map.get(key);
    }

    @SafeVarargs
    private static <T> Stream<T> concatStreams(Stream<T>... streams) {
        return Stream.of(streams).reduce(Stream::concat).get();
    }

    private static Stream<Pair<Character, Integer>> charScoreStream(String chars, Integer score) {
        return characterStream(chars)
                 .map(c -> new Pair<Character, Integer>(c, score));
    }

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
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
