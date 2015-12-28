import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.IntUnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class PascalsTriangle {

    public static int[][] computeTriangle(int rows) {
        if (rows < 0) throw new IllegalArgumentException("must not be negative: " + rows);
        List<Integer> firstRow = new ArrayList<>(Arrays.asList(1));
        List<List<Integer>> pascalsTriangle =
            unfoldRight(new Pair<Integer,List<Integer>>(rows, firstRow), nextUnfold);
        return toArray(pascalsTriangle);
    }

    private static final Function<Pair<Integer,List<Integer>>,
            Optional<Pair<List<Integer>, Pair<Integer,List<Integer>>>>> nextUnfold =
        pair -> {
            int rowNumber = pair.first;
            if (rowNumber == 0) return Optional.empty();
            else {
                List<Integer> row = pair.second;
                List<Integer> nextRow = nextRow(row);
                return Optional.of(
                    new Pair<List<Integer>, Pair<Integer,List<Integer>>>(row,
                        new Pair<Integer,List<Integer>>(rowNumber - 1, nextRow)));
            }
        };

    public static boolean isTriangle(int[][] triangle) {
        int[][] pascalsTriangle = computeTriangle(triangle.length);
        return Objects.deepEquals(triangle, pascalsTriangle);
    }

    private static List<Integer> nextRow(List<Integer> row) {
        return IntStream.rangeClosed(0, row.size())
                 .map(sumOfNeighbors(row))
                 .mapToObj(Integer::new)
                 .collect(Collectors.toList());
    }

    private static IntUnaryOperator sumOfNeighbors(List<Integer> row) {
        return position -> {
            int left = position == 0 ? 0 : row.get(position - 1);
            int right = position < row.size() ?  row.get(position) : 0;
            return left + right;
        };
    }

    // nasty business
    private static int[][] toArray(List<List<Integer>> pascalsTriangle) {
        return pascalsTriangle.stream()
          .map(row -> row.stream().mapToInt(i->i).toArray())
          .collect(Collectors.toList()).toArray(new int[0][]);
    }

    private static <A, B> List<A> unfoldRight(B seed, Function<B, Optional<Pair<A, B>>> f) {
        List<A> result = new ArrayList<>();
        Optional<Pair<A, B>> maybeNext = f.apply(seed);
        while (maybeNext.isPresent()) {
            Pair<A, B> next = maybeNext.get();
            result.add(next.first);
            maybeNext = f.apply(next.second);
        }
        return result;
    }

    private static class Pair<F,S> {
        private F first;
        private S second;

        private Pair(F first, S second) {
            this.first = first;
            this.second = second;
        }
    }
}
