import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DNA {
    private String dna;

    public DNA(String dna) {
        this.dna = dna;
    }

    public Integer count(Character nucleotide) {
        if (!Nucleotides.containsKey(nucleotide))
            throw new IllegalArgumentException("not a nucleotide: " + nucleotide);
        long count = nucleotideStream().filter(c -> c == nucleotide).count();
        return new Long(count).intValue();
    }

    public Map<Character,Integer> nucleotideCounts() {
        Map<Character, Integer> nucleotideCounts = nucleotideStream()
                 .collect(Collectors.groupingBy(Function.identity(), intCounting));
        return withZeroCounts(nucleotideCounts);
    }

    private Collector<Character, ?, Integer> intCounting =
        Collectors.reducing(0, e -> 1, (a,b) -> a + b);

    private Map<Character, Integer> withZeroCounts(Map<Character, Integer> nucleotideCounts) {
        Map<Character,Integer> result = new HashMap<>(Nucleotides);
        result.putAll(nucleotideCounts);
        return result;
    }

    private Stream<Character> nucleotideStream() {
        return dna.chars().mapToObj(i -> (char)i);
    }

    @SuppressWarnings("serial")
    private static final Map<Character,Integer> Nucleotides =
        Collections.unmodifiableMap(new HashMap<Character,Integer>(){{
            put('A',0);
            put('C',0);
            put('G', 0);
            put('T', 0);
        }});
}
