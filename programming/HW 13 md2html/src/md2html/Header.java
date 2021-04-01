package md2html;

class Header {
    private StringBuilder string;

    Header(StringBuilder string) {
        this.string = string;
    }

    private int headerLvl(StringBuilder text) {
        int i = 0;
        while (i < text.length() && text.charAt(i) == '#') {
            i++;
        }
        return i;
    }

    void toHtml(StringBuilder answer) {
        int lvl = headerLvl(string);
        answer.append('<').append('h').append(lvl).append('>');
        new Text(new StringBuilder(string.substring(lvl + 1))).toHtml(answer);
        answer.append('<').append('/').append('h').append(lvl).append('>');
    }
}