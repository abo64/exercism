import static java.lang.Math.ceil;
import static java.lang.Math.sqrt;

import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Crypto {

    private String plainText;

    public Crypto(String plainText) {
        this.plainText = plainText;
    }

    public String getNormalizedPlaintext() {
        Stream<Character> normalizedPlainTextChars =
            characterStream(plainText)
              .filter(Character::isLetterOrDigit)
              .map(Character::toLowerCase);
        return mkString(normalizedPlainTextChars, "");
    }

    public int getSquareSize() {
        int textLength = getNormalizedPlaintext().length();
        return (int)ceil(sqrt(textLength));
    }

    public List<String> getPlaintextSegments() {
        String normalizedPlainText = getNormalizedPlaintext();
        return grouped(normalizedPlainText, getSquareSize());
    }

    public String getCipherText() {
        return mkString(cipherText(), "");
    }

    public String getNormalizedCipherText() {
        return mkString(cipherText(), " ");
    }

    private Stream<StringBuilder> cipherText() {
        List<String> plaintextSegments = getPlaintextSegments();
        IntStream squareColumns = IntStream.range(0, getSquareSize());
        return squareColumns
                 .mapToObj(col -> foldSquareRows(plaintextSegments, col));
    }

    private static StringBuilder foldSquareRows(List<String> plaintextSegments, int col) {
        BiFunction<StringBuilder, String, StringBuilder> addRowChar =
            (cipher, row) -> {
                if (col >= row.length()) return cipher;
                else return cipher.append(row.charAt(col));
            };
            return plaintextSegments.stream()
                     .reduce(new StringBuilder(), addRowChar, first());
    }

    private static <T> BinaryOperator<T> first() {
        return (_1, _2) -> _1;
    }

    private static <A> String mkString(Stream<A> as, CharSequence delimiter) {
        return as.map(Object::toString)
                 .collect(Collectors.joining(delimiter));
    }

    private static List<String> grouped(String str, int groupSize) {
        String regex = String.format("(?<=\\G.{%d})", groupSize);
        return Arrays.asList(str.split(regex));
    }

    private static Stream<Character> characterStream(CharSequence str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
