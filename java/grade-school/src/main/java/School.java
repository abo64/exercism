import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class School {

    private static final List<String> NoStudents =
        Collections.emptyList();

    // modifiable for simplicity and performance
    private final Map<Integer,List<String>> DB =
        new TreeMap<>();

    public Map<Integer, List<String>> db() {
        return Collections.unmodifiableMap(DB);
    }

    public void add(String student, Integer grade) {
        List<String> oldStudents = this.grade(grade);
        List<String> newStudents = addElementSorted(oldStudents, student);
        DB.put(grade, newStudents);
    }

    public List<String> grade(Integer grade) {
        return DB.getOrDefault(grade, NoStudents);
    }

    public Map<Integer, List<String>> sort() {
        return db();
    }

    // FP principle: no outside side effects, always work w/ copies
    private static <T extends Comparable<? super T>> List<T> addElementSorted(List<T> list, T element) {
        List<T> newSortedList = new ArrayList<>(list);
        newSortedList.add(element);
        Collections.sort(newSortedList);
        return Collections.unmodifiableList(newSortedList);
    }
}
