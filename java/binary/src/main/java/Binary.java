import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Binary {

    private final String input;

    public Binary(String input) {
        this.input = input;}

    public int getDecimal() {
        return parse(input).orElse(0);
    }

    private static Optional<Integer> parse(String binary) {
        return Optional.ofNullable(binary).filter(isValidBinary).map(toDecimal);
    }

    private static BiFunction<Integer,Character,Integer> nextDigit =
        (output, bin) -> output * 2 + Character.digit(bin, 10);

    private static Function<String,Integer> toDecimal =
        binary -> foldLeft(characterStream(binary), 0, nextDigit);

    private static Predicate<String> isValidBinary =
        candidate ->
            !candidate.isEmpty() &&
            characterStream(candidate).allMatch(c -> c == '0' || c == '1');

    private static <A,B> B foldLeft(Stream<A> as, B seed, BiFunction<B,A,B> f) {
        List<A> list = as.collect(Collectors.toList());
        return foldLeft(list, seed, f);

//        final Iterator<A> iter = as.iterator();
//        B result = seed;
//        while (iter.hasNext()) {
//            result = f.apply(result, iter.next());
//        }
//        return result;
    }

    private static <A,B> B foldLeft(List<A> as, B seed, BiFunction<B,A,B> f) {
        if (as.isEmpty()) return seed;
        else {
            A head = as.get(0);
            List<A> tail = as.subList(1, as.size());
            return foldLeft(tail, f.apply(seed, head), f);
        }
    }

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
