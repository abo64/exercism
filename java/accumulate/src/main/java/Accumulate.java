import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;

public class Accumulate {

    public static <T> List<T> accumulate(List<T> ts, Function<T, T> f) {
        return ts.stream()
                 .reduce(new ArrayList<>(), map(f), first());
    }

    private static <T> BiFunction<List<T>,T,List<T>> map(Function<T, T> f) {
        return (fts, t) -> {
            fts.add(f.apply(t));
            return fts;
        };
    }

    private static <T> BinaryOperator<T> first() {
        return (_1, _2) -> _1;
    }
}
