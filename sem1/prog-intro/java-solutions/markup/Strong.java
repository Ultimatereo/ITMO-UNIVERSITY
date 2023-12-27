package markup;

import java.util.List;

public class Strong extends AbstractMark {
    private static List<String> key = List.of("__", "strong");
    public Strong(List<Element> list) {
        super(key, list);
    }
}
