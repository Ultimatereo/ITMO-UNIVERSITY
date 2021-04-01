package md2html;

class Paragraph {
    private StringBuilder string;

    Paragraph(StringBuilder string) {
        this.string = string;
    }

    void toHtml(StringBuilder ans) {
        ans.append('<').append('p').append('>');
        new Text(string).toHtml(ans);
        ans.append('<').append('/').append('p').append('>');
    }
}
