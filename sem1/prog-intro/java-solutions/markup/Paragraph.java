package markup;

import java.util.List;

public class Paragraph implements Markable, Htmlable {
    private final List<Element> list;

    public Paragraph(List<Element> list) {
        this.list = list;
    }

    @Override
    public void toMarkdown(StringBuilder sb) {
        for (Element element : list) {
            element.toMarkdown(sb);
        }
    }

    @Override
    public void toHtml(StringBuilder sb) {
        for (Element element : list) {
            element.toHtml(sb);
        }
    }
}
