import java.util.HashSet;
import java.util.Random;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Robot {

    private String name = randomName();

    public String getName() { return this.name; }

    public void reset() { this.name = randomName(); }

    private static String randomName() {
        return randomNames()
                .filter(Robot::isUnusedName)
                .findFirst().get();
    }

    private static final Supplier<String> RandomNameSupplier =
        new Supplier<String>() {
            @Override public String get() {
                return twoRandomUpperLetters() + threeRandomIntegers();
            }};

    private static Stream<String> randomNames() {
        return Stream.generate(RandomNameSupplier);
    }

    private static String twoRandomUpperLetters() {
        return randomChars(2, 'A', 'Z');
    }

    private static String threeRandomIntegers() {
        return randomChars(3, '0', '9');
    }

    private static String randomChars(int howMany, char lowerBound, char upperBound) {
        Stream<Character> randomUpperChars =
            new Random().ints(lowerBound, upperBound + 1)
              .mapToObj(i -> (char)i)
              .limit(howMany);
        return mkString(randomUpperChars);
    }

    private static <A> String mkString(Stream<A> as) {
        return as.map(Object::toString)
                 .collect(Collectors.joining(""));
    }

    private static Set<String> usedNames = new HashSet<>();
    private static boolean isUnusedName(String candidate) {
        if (usedNames.contains(candidate)) return false;

        usedNames.add(candidate);
        return true;
	}
}
