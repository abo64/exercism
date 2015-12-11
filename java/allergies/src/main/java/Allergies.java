import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Allergies {

    private final int score;

    public Allergies(int score) {
        this.score = score;
    }

    public boolean isAllergicTo(Allergen allergen) {
        return (allergen.getScore() & score) == allergen.getScore();
    }

    public List<Allergen> getList() {
            return Arrays.stream(Allergen.values())
                     .filter(a -> isAllergicTo(a))
                     .collect(Collectors.toList());
    }

}