package markup;

import java.util.List;
import java.util.Map;

public class AbstractMark implements Element {
    protected List<String> key;
    protected List<Element> list;

    public AbstractMark(List<String> key, List<Element> list) {
        this.key = key;
        this.list = list;
    }

    @Override
    public void toMarkdown(StringBuilder sb) {
        String mark = key.get(0);
        sb.append(mark);
        for (Element element : list) {
            element.toMarkdown(sb);
        }
        sb.append(mark);
    }

    @Override
    public void toHtml(StringBuilder sb) {
        String mark = key.get(1);
        sb.append("<").append(mark).append(">");
        for (Element element : list) {
            element.toHtml(sb);
        }
        sb.append("</").append(mark).append(">");
    }
}
