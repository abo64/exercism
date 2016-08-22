import java.util.Formatter;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class BeerSong {

    public static String verse(int whichOne) {
        return new Verse(whichOne).toString();
    }

    public static String sing(int from, int to) {
        Stream<Verse> verses =
            IntStream.rangeClosed(-from, -to)  // there seems to be no range /w step = -1
                .mapToObj(i -> new Verse(-i));
        return mkString(verses);
    }

    public static String singSong() {
        return sing(99, 0);
    }

    private static <A> String mkString(Stream<A> as) {
        return as.map(Object::toString)
                 .collect(Collectors.joining(""));
    }

    private static String printf(String format, Object ... args) {
        Formatter formatter = new Formatter();
        try {
            return formatter.format(format, args).toString();
        } finally { formatter.close(); }
    }

    private static class Verse {
        private final int whichOne;

        private Verse(int whichOne) {
            this.whichOne = whichOne;
        }

        private static final String MANY_BOTTLES_FORMAT =
            "%s of beer on the wall, %s of beer.\nTake %s down and pass it around, %s of beer on the wall.\n\n";

        private static final String NO_BOTTLES_VERSE =
            "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n\n";

        @Override
        public String toString() {
            if (whichOne > 0) {
                Bottles bottles = new Bottles(whichOne);
                Take take = new Take(whichOne);
                Bottles oneBottleLess = new Bottles(whichOne - 1);
                return printf(MANY_BOTTLES_FORMAT, bottles, bottles, take, oneBottleLess);
            }
            else
                return NO_BOTTLES_VERSE;
        }
    }

    private static class Take {
        private final int howMany;

        private Take(int howMany) {
            this.howMany = howMany;
        }

        @Override
        public String toString() {
            return howMany > 1 ? "one" : "it";
        }
    }

    private static class Bottles {
        private final int howMany;

        private Bottles(int howMany) {
            this.howMany = howMany;
        }

        @Override
        public String toString() {
            if (howMany > 1) return howMany + " bottles";
            else if (howMany == 1) return "1 bottle";
            else return "no more bottles";
        }
    }
}