import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Function;

public class Accumulate {

    public static <T> List<T> accumulate(List<T> ts, Function<T, T> f) {
        return ts.stream()
                 .collect(ArrayList::new, map(f), ArrayList::addAll);
    }

    private static <T> BiConsumer<ArrayList<T>,T> map(Function<T, T> f) {
        return (fts, t) -> {
            fts.add(f.apply(t));
        };
    }
}
