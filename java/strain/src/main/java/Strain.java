import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Predicate;

public class Strain {

    public static <T> List<T> keep(List<T> ts, Predicate<T> p) {
        return ts.stream()
                 .collect(ArrayList::new, addIf(p), List::addAll);
    }

    private static <T> BiConsumer<List<T>,T> addIf(Predicate<T> p) {
        return (filteredTs, t) -> {
                  if (p.test(t)) filteredTs.add(t);
               };
    }

    public static <T> List<T> discard(List<T> ts, Predicate<T> p) {
        return keep(ts, p.negate());
    }
}
