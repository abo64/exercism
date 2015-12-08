import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Atbash {

    public static String encode(String plainText) {
        return characterStream(plainText.toLowerCase())
                .filter(Character::isLetterOrDigit)
                .map(atbashEncode)
                .collect(StringBuilder::new,
                         grouped(5),
                         StringBuilder::append)
               .toString();
    }

    public static String decode(String cipherText) {
        return characterStream(cipherText)
                 .filter(c -> c != ' ')
                 .map(atbashDecode)
                 .collect(StringBuilder::new,
                          (sb, i) -> sb.append((char)i),
                          StringBuilder::append)
                .toString();
    }

    private static BiConsumer<StringBuilder, Character> grouped(int groupSize) {
        // not really elegant: is there a better way to group a Character Stream?
        AtomicInteger dynamicGroupSize = new AtomicInteger(groupSize);
        return (sb, i) -> {
            int length = sb.length();
            if (length > 0 && length % dynamicGroupSize.get() == 0) {
                sb.append(' ');
                dynamicGroupSize.addAndGet(groupSize + 1);
            }
            sb.append((char) i);
        };
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
