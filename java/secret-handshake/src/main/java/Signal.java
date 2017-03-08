import java.util.List;
import java.util.function.Predicate;

enum Signal {

    WINK(1), DOUBLE_BLINK(10), CLOSE_YOUR_EYES(100), JUMP(1000),
    REVERSE(10000) {
        @Override public List<Signal> operation(List<Signal> signals) {
            return Lists.reverse(signals);
        }
    };

    public static Predicate<Signal> applies(int input) {
        return s -> (input & s.getIntCode()) > 0;
    }

    private final int binCode;

    private Signal(int binCode) {
        this.binCode = binCode;
    }

    private int getIntCode() {
        return Integer.parseInt(Integer.toString(binCode), 2);
    }

    public List<Signal> operation(List<Signal> signals) {
        return Lists.add(this, signals);
    }
}
