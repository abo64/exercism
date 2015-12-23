import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

public class RomanNumeral {

    private final int arabicNumber;

    public RomanNumeral(int arabicNumber) {
        this.arabicNumber = arabicNumber;
    }

    public String getRomanNumeral() {
        return toRoman(arabicNumber);
    }

    private static String toRoman(int arabicNumber) {
        List<String> romanNumberParts = unfoldRight(arabicNumber, findNext);
        Optional<String> romanNumeral =
            romanNumberParts.stream().reduce(String::concat);
        return romanNumeral.orElse("");
    }

    private static final Function<Integer,Optional<Pair<String,Integer>>> findNext =
        arabicNumber -> {
            Optional<Pair<Integer, String>> next =
              arabicToRoman()
                .filter(pair -> pair.first <= arabicNumber)
                .findFirst();
            return next.map(found ->
                     new Pair<String,Integer>(found.second, arabicNumber - found.first));
        };

    // more efficient and easier to understand than returning a Stream?!
    private static <A, B> List<A> unfoldRight(B seed, Function<B, Optional<Pair<A, B>>> f) {
        List<A> result = new ArrayList<>();
        Optional<Pair<A, B>> maybeNext = f.apply(seed);
        while (maybeNext.isPresent()) {
            Pair<A, B> next = maybeNext.get();
            result.add(next.first);
            maybeNext = f.apply(next.second);
        }
        return result;
    }

//    private static <A,B> Stream<A> unfoldRight(B seed, Function<B, Optional<Pair<A,B>>> f) {
//        Optional<Pair<A, B>> next = f.apply(seed);
//        return next
//                 .map(pair -> Stream.concat(Stream.of(pair.first), unfoldRight(pair.second, f)))
//                 .orElse(Stream.empty());
//    }

    private static Stream<Pair<Integer,String>> arabicToRoman() {
        return Stream.of(
          lexicon(1000, "M"),
          lexicon(900, "CM"),
          lexicon(500, "D"),
          lexicon(400, "CD"),
          lexicon(100, "C"),
          lexicon(90 , "XC"),
          lexicon(50 , "L"),
          lexicon(40 , "XL"),
          lexicon(10 , "X"),
          lexicon(9  , "IX"),
          lexicon(5  , "V"),
          lexicon(4  , "IV"),
          lexicon(1  , "I"));
    }

    private static Pair<Integer,String> lexicon(Integer arabicNumber, String romanLiteral) {
        return new Pair<Integer,String>(arabicNumber, romanLiteral);
    }

    private static class Pair<F,S> {
        private F first;
        private S second;

        private Pair(F first, S second) {
            this.first = first;
            this.second = second;
        }
    }
}
