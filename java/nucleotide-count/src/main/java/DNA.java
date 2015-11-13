import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
        if (!Nucleotides.contains(nucleotide))
            throw new IllegalArgumentException("not a nucleotide: " + nucleotide);
        long count = nucleotideStream().filter(c -> c == nucleotide).count();
        return new Long(count).intValue();
    }

    public Map<Character,Integer> nucleotideCounts() {
        Map<Character, List<Character>> charsByChar =
            nucleotideStream().collect(Collectors.groupingBy(Function.identity()));
        Map<Character, Integer> nucleotideCounts = mapValues(charsByChar, l -> l.size());
        putZerosIfAbsent(nucleotideCounts);
        return nucleotideCounts;
    }

    private Stream<Character> nucleotideStream() {
        return dna.chars().mapToObj(i -> (char)i);
    }

    private static final Set<Character> Nucleotides =
      new HashSet<>(Arrays.asList('A', 'C', 'G', 'T'));

    private static final void putZerosIfAbsent(Map<Character,Integer> nucleotideCounts) {
        Nucleotides.forEach(nucleotide -> nucleotideCounts.putIfAbsent(nucleotide, 0));
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
