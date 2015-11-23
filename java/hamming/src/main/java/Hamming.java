import java.util.Iterator;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class Hamming {

    public static int compute(String strand1, String strand2) {
        if (strand1.length() != strand2.length())
            throw new IllegalArgumentException("different strand lengths");

        Stream<NucleotidePair> nucleotidePairs = pairStrands(strand1, strand2);
        long count =
            nucleotidePairs
                .filter(NucleotidePair::differentNucleotides)
                .count();
        return (int)count;
    }

    private static Stream<NucleotidePair> pairStrands(String strand1, String strand2) {
        return zip(characterStream(strand1), characterStream(strand2))
                 .map(NucleotidePair::new);
    }

    private static <A,B> Stream<Pair<A,B>> zip(Stream<A> as, Stream<B> bs) {
        // is there a better way than using an iterator?
        Iterator<B> bIter = bs.iterator();
        return as.map(a -> bIter.hasNext() ? new Pair<A,B>(a, bIter.next()) : null)
                 .filter(notNull);
    }

    private static final Predicate<Object> notNull = o -> o != null;

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }

    private static class NucleotidePair extends Pair<Character, Character> {
        private NucleotidePair(Pair<Character, Character> pair) {
            super(pair.first, pair.second);
        }

        boolean differentNucleotides() {
            return getFirst() != getSecond();
        }
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
