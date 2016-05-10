import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RnaTranscription {

    public static Object ofDna(String dna) {
        Stream<Character> rnaNucleotides =
           characterStream(dna).map(RnaMap::get);

        return mkString(rnaNucleotides, "");
    }

    private static final Map<Character, Character> RnaMap = new HashMap<>();
    static {
        RnaMap.put('G', 'C');
        RnaMap.put('C', 'G');
        RnaMap.put('T', 'A');
        RnaMap.put('A', 'U');
    }

    private static <A> String mkString(Stream<A> as, CharSequence delimiter) {
        return as.map(Object::toString)
                 .collect(Collectors.joining(delimiter));
    }

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
