import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Predicate;

public class Strain {

    public static <T> List<T> keep(List<T> ts, Predicate<T> p) {
        return ts.stream()
                 .reduce(new ArrayList<>(), addIf(p), first());
    }

    private static <T> BiFunction<List<T>,T,List<T>> addIf(Predicate<T> p) {
        return (filteredTs, t) -> {
                  if (p.test(t)) filteredTs.add(t);
                  return filteredTs;
               };
    }

    private static <T> BinaryOperator<T> first() {
        return (_1, _2) -> _1;
    }

    public static <T> List<T> discard(List<T> ts, Predicate<T> p) {
        return keep(ts, p.negate());
    }
}
