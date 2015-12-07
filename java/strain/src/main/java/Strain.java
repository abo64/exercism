import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public class Strain {

    public static <T> List<T> keep(List<T> ts, Predicate<T> p) {
        Optional<T> maybeHead = head(ts);
        if (!maybeHead.isPresent()) {
            return Collections.emptyList();
        } else {
            T head = maybeHead.get();
            List<T> tail = tail(ts).get();
            List<T> keepTail = keep(tail, p);
            return p.test(head) ?
                cons(head, keepTail) :
                keepTail;
        }
    }

    public static <T> List<T> discard(List<T> ts, Predicate<T> p) {
        return keep(ts, p.negate());
    }

    private static <T> List<T> cons(T t, List<T> ts) {
        List<T> result = new ArrayList<>(ts);
        result.add(0, t);
        return result;
    }

    private static <T> Optional<T> head(List<T> ts) {
        if (ts.isEmpty()) return Optional.empty();
        else return Optional.of(ts.get(0));
    }

    private static <T> Optional<List<T>> tail(List<T> ts) {
        if (ts.isEmpty()) return Optional.empty();
        else return Optional.of(ts.subList(1, ts.size()));
    }
}
