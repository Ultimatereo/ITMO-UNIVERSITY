package md2html;

import java.util.HashMap;
import java.util.Map;

class Text {

    private static Map<String, Integer> mdIndex;
    private static String[] htmlKey;
    private static String[] mdKey;

    static {
        mdKey = new String[]{"*", "_", "**", "__", "--", "`", "~"};
        mdIndex = new HashMap<>();
        int keyCount = mdKey.length;
        for (int i = 0; i < keyCount; i++) {
            mdIndex.put(mdKey[i], i);
        }
        htmlKey = new String[]{"em", "em", "strong", "strong", "s", "code", "mark"};
    }

    private StringBuilder string;

    Text(StringBuilder string) {
        this.string = string;
    }

    private Integer getKeyPos(String current) {
        Integer result = mdIndex.get(current);
        if (result == null) {
            result = mdIndex.get(Character.toString(current.charAt(0)));
        }
        return result;
    }

    void toHtml(StringBuilder answer) {
        Map<Character, String> symbols = Map.of('<', "&lt;", '>', "&gt;", '&', "&amp;");
        String value;
        int KeyCount = htmlKey.length;
        StackAN[] array = new StackAN[KeyCount];
        for (int i = 0; i < KeyCount; i++) {
            array[i] = new StackAN();
        }
        for (int i = 0; i < string.length(); i++) {
            String current;
            if (i + 2 < string.length()) {
                current = string.substring(i, i + 2);
            } else {
                current = string.substring(i, string.length());
            }
            if (string.charAt(i) == '\\') {
                i++;
                continue;
            }
            Integer pos = getKeyPos(current);
            if (pos != null) {
                array[pos].add(i);
            }
        }
        for (StackAN j : array) {
            if (j.getSize() % 2 == 1) {
                j.pop();
            }
        }
        int[] pos = new int[KeyCount];
        for (int i = 0; i < string.length(); i++) {
            char c = string.charAt(i);
            String cur;
            if (i + 2 < string.length()) {
                cur = string.substring(i, i + 2);
            } else {
                cur = string.substring(i, string.length());
            }
            if (c == '\\') {
                continue;
            } else if (symbols.get(c) != null) {
                answer.append(symbols.get(c));
                continue;
            }
            Integer Pos = getKeyPos(cur);
            if (Pos == null || array[Pos].getSize() == 0) {
                answer.append(c);
            } else {
                cur = mdKey[Pos];
                String keyName = htmlKey[Pos];
                int keySize = cur.length();
                boolean isOpen = ((array[Pos].getSize() - pos[Pos]) % 2 == 0);
                String tmp;
                if (isOpen) {
                    tmp = "<" + "" + keyName + ">";
                } else {
                    tmp = "<" + "/" + keyName + ">";
                }
                answer.append(tmp);
                i += keySize - 1;
                pos[Pos]++;
            }
        }
    }
}
