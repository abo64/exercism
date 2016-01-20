import java.util.Optional;

public class Deque<T> {

    Optional<Element<T>> first = Optional.empty();
    Optional<Element<T>> last = Optional.empty();

    public void push(T t) {
        Optional<Element<T>> newElement =
            Optional.of(new Element<T>(t, last, Optional.empty()));
        last.ifPresent(l -> l.next = newElement);
        last = newElement;
        first = first.isPresent() ? first : last;
    }

    public T pop() {
        Optional<T> result = last.map(l -> l.value);
        last = last.flatMap(l -> l.prev);
        return result.get(); // more natural would be to return the Optional itself
    }

    public void unshift(T t) {
        Optional<Element<T>> newElement =
           Optional.of(new Element<T>(t, Optional.empty(), first));
        first.ifPresent(f -> f.prev = newElement);
        first = newElement;
        last = last.isPresent() ? last : first;
    }

    public T shift() {
        Optional<T> result = first.map(f -> f.value);
        first = first.flatMap(f -> f.next);
        return result.get(); // again, more natural would be to return the Optional itself
    }

    private static class Element<T> {
        private T value;
        private Optional<Element<T>> prev;
        private Optional<Element<T>> next;

        private Element(T value, Optional<Element<T>> prev, Optional<Element<T>> next) {
            this.value = value;
            this.next = next;
            this.prev = prev;
        }
    }
}
