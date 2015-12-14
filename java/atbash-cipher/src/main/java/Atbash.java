import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Atbash {

    public static String encode(String plainText) {
        Stream<Character> encodedStream =
            characterStream(plainText.toLowerCase())
                .filter(Character::isLetterOrDigit)
                .map(atbashEncode);

        List<String> groups = grouped(mkString(encodedStream), 5);
        return groups.stream().collect(Collectors.joining(" "));
    }

    public static String decode(String cipherText) {
        Stream<Character> decodedStream =
            characterStream(cipherText)
                 .filter(c -> c != ' ')
                 .map(atbashDecode);
        return mkString(decodedStream);
    }

    private static List<String> grouped(String str, int groupSize) {
        String regex = String.format("(?<=\\G.{%d})", groupSize);
        return Arrays.asList(str.split(regex));
    }

    private static final Map<Character, Character> AtbashEncodeMap =
        zip(alphabet(), alphabet().sorted(Comparator.reverseOrder()))
          .collect(pairsToMap());
    private static final Function<Character, Character> atbashEncode =
            mapToFunction(AtbashEncodeMap, Function.identity());


    private static final Map<Character, Character> AtbashDecodeMap =
        mapToPairStream(AtbashEncodeMap)
          .map(Pair::flip)
          .collect(pairsToMap());
    private static final Function<Character, Character> atbashDecode =
            mapToFunction(AtbashDecodeMap, Function.identity());


    private static <K, V> Function<K, V> mapToFunction(Map<K, V> map, Function<K,V> computeIfAbsent) {
        return key -> map.getOrDefault(key, computeIfAbsent.apply(key));
    }

    private static Stream<Character> alphabet() {
        return IntStream.rangeClosed('a', 'z').mapToObj(i -> (char)i);
    }

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }

    private static <A> String mkString(Stream<A> as) {
        return as.map(Object::toString)
                 .collect(Collectors.joining(""));
    }

    private static <K, V> Stream<Pair<K, V>> mapToPairStream(Map<K, V> source) {
        return source.entrySet().stream()
                 .map(e -> new Pair<K,V>(e.getKey(), e.getValue()));
    }

    private static final <K, V> Collector<Pair<K, V>, ?, Map<K, V>> pairsToMap() {
        return Collectors.toMap(Pair::getFirst, Pair::getSecond);
    }

    private static <A,B> Stream<Pair<A,B>> zip(Stream<A> as, Stream<B> bs) {
        // is there a better way than using an iterator?
        Iterator<B> bIter = bs.iterator();
        return as.map(a -> bIter.hasNext() ? new Pair<A,B>(a, bIter.next()) : null)
                 .filter(o -> o != null);
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

        public Pair<S,F> flip() { return new Pair<S,F>(second, first); }
    }
}
