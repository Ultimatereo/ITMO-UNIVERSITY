package markup;

public class Text implements Element {
    private final String string;

    public Text(String string) {
        this.string = string;
    }

    @Override
    public void toMarkdown(StringBuilder sb) {
        sb.append(string);
    }

    @Override
    public void toHtml(StringBuilder sb) {
        sb.append(string);
    }
}
