package md2html;

public class Header {
    private final StringBuilder input;

    public Header(StringBuilder input) {
        this.input = input;
    }


    public void md2Html(StringBuilder output, int lvl) {
        output.append("<h").append(lvl).append(">");
        new Text(input).md2Html(output, lvl + 1);
        output.append("</h").append(lvl).append(">").append('\n');
    }
}
