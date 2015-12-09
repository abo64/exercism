import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Predicate;

public class Strain {

    public static <T> List<T> keep(List<T> ts, Predicate<T> p) {
        return foldLeft(ts,
                 new ArrayList<T>(),
                 (filteredTs, t) -> {
                     if (p.test(t)) filteredTs.add(t);
                     return filteredTs;
                 });
    }

    public static <T> List<T> discard(List<T> ts, Predicate<T> p) {
        return keep(ts, p.negate());
    }

    private static <A, B> B foldLeft(Iterable<A> as, B z, BiFunction<B,A,B> f) {
        B result = z;
        for (A a : as) {
            result = f.apply(result, a);
        }
        return result;
    }
}
