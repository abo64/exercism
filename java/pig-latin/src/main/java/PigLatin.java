import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class PigLatin {

    public static String translate(String clearText) {
        Stream<String> words = Stream.of(clearText.split(" "));
        return mkString(words.map(wordToPigLatin), " ");
    }

    private static final String PigEnding = "ay";

    private static final String Vowels = "aeiou";
    private static final Pattern BeginsWithVowel;
    static {
        String vowel = "(xr|yt|[" + Vowels + "])";
        BeginsWithVowel = Pattern.compile(vowel + "(.*)");
    }
    private static final Pattern BeginsWithConsonant;
    static {
        String consonants = "(qu|squ|[^" + Vowels + "]+)";
        BeginsWithConsonant = Pattern.compile(consonants + "(.*)");
    }

    private static final Function<String,String> wordToPigLatin =
        word -> {
            Matcher vowelMatcher = BeginsWithVowel.matcher(word);
            if (vowelMatcher.matches())
                return word + PigEnding;

            Matcher consonantMatcher = BeginsWithConsonant.matcher(word);
            if (!consonantMatcher.matches())
                throw new IllegalArgumentException("not matching word: " + word);

            String consonant = consonantMatcher.group(1);
            String rest = consonantMatcher.group(2);
            return rest + consonant + PigEnding;
        };

    private static <A> String mkString(Stream<A> as, CharSequence delimiter) {
        return as.map(Object::toString)
                 .collect(Collectors.joining(delimiter));
    }
}
