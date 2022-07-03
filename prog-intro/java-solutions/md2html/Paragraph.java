package md2html;

public class Paragraph {
    private final StringBuilder input;

    public Paragraph(StringBuilder input) {
        this.input = input;
    }

    public void md2Html(StringBuilder output) {
        output.append("<p>");
        new Text(input).md2Html(output, 0);
        output.append("</p>").append('\n');
    }
}
