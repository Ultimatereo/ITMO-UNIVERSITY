package markup;

import java.util.List;

public class Emphasis extends AbstractMark {
    private static List<String> key = List.of("*", "em");
    public Emphasis(List<Element> list) {
        super(key, list);
    }
}
