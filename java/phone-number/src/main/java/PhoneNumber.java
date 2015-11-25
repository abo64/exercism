import java.util.Optional;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class PhoneNumber {

    private static final PhoneNumberParts InvalidPhoneNumberParts =
        new PhoneNumberParts("000", "000", "0000");

    private final PhoneNumberParts phoneNumberParts;

    public PhoneNumber(String phoneNumber) {
        this.phoneNumberParts =
            parsePhoneNumber(phoneNumber).orElse(InvalidPhoneNumberParts);
    }

    public String getNumber() {
        return phoneNumberParts.areaCode + phoneNumberParts.prefix +
               phoneNumberParts.lineNumber;
    }

    public String getAreaCode() {
        return phoneNumberParts.areaCode;
    }

    public String pretty() {
        return '(' + phoneNumberParts.areaCode + ") " + phoneNumberParts.prefix +
                '-' + phoneNumberParts.lineNumber;
    }

    private static final Pattern PhoneNumberPattern =
        Pattern.compile("1?(\\d{3})(\\d{3})(\\d{4})");

    private Optional<PhoneNumberParts> parsePhoneNumber(String candidate) {
        String digitsOnlyCandidate =
            filterChars(candidate, Character::isDigit);

        Matcher matcher = PhoneNumberPattern.matcher(digitsOnlyCandidate);
        if (matcher.matches()) {
            return Optional.of(new PhoneNumberParts(matcher.group(1),
                       matcher.group(2), matcher.group(3)));
        } else {
            return Optional.empty();
        }
    }

    private static class PhoneNumberParts {

        private final String areaCode;
        private final String prefix;
        private String lineNumber;

        private PhoneNumberParts(String areaCode, String prefix, String lineNumber) {
            this.areaCode = areaCode;
            this.prefix = prefix;
            this.lineNumber = lineNumber;
        }
    }

    private String filterChars(String str, Predicate<Character> filterPredicate) {
        Stream<Character> filteredChars =
            characterStream(str).filter(filterPredicate);
        return toString(filteredChars);
    }

    private static String toString(Stream<Character> characterStream) {
        return characterStream
                .collect(StringBuilder::new,
                        (sb, i) -> sb.append((char)i),
                        StringBuilder::append)
               .toString();
    }

    private static Stream<Character> characterStream(String str) {
        return str.chars().mapToObj(i -> (char)i);
    }
}
