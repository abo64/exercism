import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Predicate;

final class HandshakeCalculator {

    public List<Signal> calculateHandshake(int input) {
        Predicate<Signal> keepSignal = Signal.applies(input);

        return Enums.stream(Signal.class)
                 .filter(keepSignal)
                 .reduce(Lists.arrayList(), signalOperation, Lists::addAll);
    }

    private static final BiFunction<List<Signal>, Signal, List<Signal>> signalOperation =
        (signals, signal) -> signal.operation(signals);
}
