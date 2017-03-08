import java.util.ArrayList;
import java.util.List;

public class Lists {

    public static <T> List<T> arrayList() {
        return new ArrayList<>();
    }

    public static final <T> List<T> addAll(List<T> l1, List<T> l2) {
        l1.addAll(l2);
        return l1;
    }

    public static <T> List<T> reverse(List<T> xs) {
        return xs.stream()
                 .reduce((List<T>)new ArrayList<T>(), Functions.flip(Lists::cons), Lists::addAll);
    }

    public static <T> List<T> cons(T x, List<T> xs) {
        xs.add(0, x);
        return xs;
    }

    public static <T> List<T> add(T x, List<T> xs) {
        xs.add(x);
        return xs;
    }
}
