import java.util.Arrays;
import java.util.stream.Stream;

public class Enums {

    public static <E> Stream<E> stream(Class<E> enumClass) {
        return Arrays.stream(enumClass.getEnumConstants());
    }

}
