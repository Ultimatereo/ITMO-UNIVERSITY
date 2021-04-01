package md2html;

class Str {
    private StringBuilder string;

    Str(StringBuilder string) {
        this.string = string;
    }

    void toHtml(StringBuilder ans) {
        if (isHeader(string)) {
            new Header(string).toHtml(ans);
        } else {
            new Paragraph(string).toHtml(ans);
        }
    }

    private boolean isHeader(StringBuilder text) {
        int i = 0;
        while (i < text.length() && text.charAt(i) == '#') {
            i++;
        }
        return (i > 0 && i < text.length() && text.charAt(i) == ' ');
    }
}
