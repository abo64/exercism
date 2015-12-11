import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RnaTranscription {

    public static Object ofDna(String dna) {
        Stream<Character> rnaNucleotides =
           characterStream(dna).map(toRnaNucleotide);

        return mkString(rnaNucleotides, "");
    }

    private static final Function<Character, Character> toRnaNucleotide;
    static {
        Map<Character, Character> toRnaMap = new HashMap<>();
        toRnaMap.put('G', 'C');
        toRnaMap.put('C', 'G');
        toRnaMap.put('T', 'A');
        toRnaMap.put('A', 'U');
        toRnaNucleotide = mapToFunction(toRnaMap, Optional.empty());
    }

    private static <K,V> Function<K,V> mapToFunction(Map<K,V> map, Optional<V> defaultValue) {
        return key -> defaultValue.isPresent() ?
            map.getOrDefault(key, defaultValue.get()) : map.get(key);
    }

    private static <A> String mkString(Stream<A> as, CharSequence delimiter) {
        return as.map(Object::toString)
                 .collect(Collectors.joining(delimiter));
    }

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
