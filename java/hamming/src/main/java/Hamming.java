import java.util.function.IntPredicate;
import java.util.stream.IntStream;

public class Hamming {

    public static int compute(String strand1, String strand2) {
        if (strand1.length() != strand2.length())
            throw new IllegalArgumentException("different strand lengths");

        IntStream nucleotidePositions = IntStream.range(0, strand1.length());

        return (int)
            nucleotidePositions
              .filter(differentNucleotides(strand1, strand2))
              .count();
    }

    private static IntPredicate differentNucleotides(String strand1, String strand2) {
        return position -> strand1.charAt(position) != strand2.charAt(position);
    }
}
