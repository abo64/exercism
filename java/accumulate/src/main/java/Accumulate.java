import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

public class Accumulate {

    public static <T> List<T> accumulate(List<T> ts, Function<T, T> f) {
        return foldLeft(ts,
                 new ArrayList<T>(),
                 (fts, t) -> {
                     fts.add(f.apply(t));
                     return fts;
                 });
    }

    private static <A, B> B foldLeft(Iterable<A> as, B z, BiFunction<B,A,B> f) {
        B result = z;
        for (A a : as) {
            result = f.apply(result, a);
        }
        return result;
    }
}
