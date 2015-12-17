import java.util.Iterator;
import java.util.Random;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Cipher {

    private final String key;

    public Cipher(String key) {
        this.key = key;
        if (!isValidKey(key)) throw new IllegalArgumentException("invalid key: " + key);
    }

    private boolean isValidKey(String key) {
        return !key.isEmpty() &&
                characterStream(key)
                 .allMatch(Character::isLowerCase);
    }

    public Cipher() {
        this(randomKey());
    }

    private static final Random random = new Random();

    private static String randomKey() {
        // didn't the README.md say "at least 100 characters"?
        // well, the tests demand exactly 100 characters
        int size = 100; // + random.nextInt(100);
        return mkString(random.ints(LowerBound, UpperBound + 1)
                          .mapToObj(i -> (char)i)
                          .limit(size));
    }

    public String getKey() {
        return key;
    }

    private static final Function<Integer, Integer> Right =
        Function.identity();
    private static final Function<Integer, Integer> Left =
        n -> -n;


    public String encode(String clearText) {
        return shiftText(clearText, Right);
    }

    public String decode(String cipherText) {
        return shiftText(cipherText, Left);
    }

    private static final int LowerBound = 'a';
    private static final int UpperBound = 'z';
    private static final int MaxOffset = 25;

    private String shiftText(String text, Function<Integer, Integer> direction) {
        Function<Pair<Character,Integer>, Character> shiftCar =
            pair -> {
                int amount = pair.first - LowerBound + direction.apply(pair.second);

                int cyclicShifted = LowerBound + amount;
                if (amount < 0)
                    cyclicShifted = UpperBound + amount + 1;
                else if (amount > MaxOffset)
                    cyclicShifted = LowerBound + amount - MaxOffset - 1;

                return (char)cyclicShifted;
            };

        return mkString(
                zip(characterStream(text), keyOffsets())
                 .map(shiftCar));
    }

    private Stream<Integer> keyOffsets() {
        IntStream positiveInts = IntStream.range(0, Integer.MAX_VALUE);

        int keyLength = key.length();
        IntFunction<Integer> keyCharDistanceFromLetterA =
            index -> key.charAt(index % keyLength) - 'a';

        return positiveInts.mapToObj(keyCharDistanceFromLetterA);
    }

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }

    private static <A> String mkString(Stream<A> as) {
        return as.map(Object::toString)
                 .collect(Collectors.joining(""));
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
    }
}
