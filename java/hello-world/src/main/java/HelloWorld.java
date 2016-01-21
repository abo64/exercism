import java.util.Optional;
import java.util.function.Predicate;

public class HelloWorld {

    public static String hello(String maybeName) {
        String name =
            Optional.ofNullable(maybeName)
              .filter(isBlank.negate())
              .orElse("World");

        return String.format("Hello, %s!", name);
    }

    private static final Predicate<String> isBlank =
        str -> str.trim().equals("");
}
