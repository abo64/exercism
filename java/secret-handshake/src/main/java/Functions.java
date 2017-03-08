import java.util.function.BiFunction;

public class Functions {

    public static <A,B,C> BiFunction<B,A,C> flip(BiFunction<A,B,C> f) {
        return (b, a) -> f.apply(a, b);
    }
}
