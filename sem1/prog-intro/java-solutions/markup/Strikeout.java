package markup;

import java.util.List;

public class Strikeout extends AbstractMark {
    private static List<String> key = List.of("~", "s");
    public Strikeout(List<Element> list) {
        super(key, list);
    }
}
