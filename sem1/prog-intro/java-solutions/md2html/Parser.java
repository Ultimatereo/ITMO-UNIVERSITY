package md2html;

public class Parser {
    private final StringBuilder input;

    public Parser(StringBuilder input) {
        this.input = input;
    }

    public void md2Html(StringBuilder output) {
        int i = 0;
        while (i < input.length() && input.charAt(i) == '#') {
            i++;
        }

        if (i > 0 && i < input.length() && input.charAt(i) == ' ') {
            new Header(input).md2Html(output, i);
        } else {
            new Paragraph(input).md2Html(output);
        }
    }

}
