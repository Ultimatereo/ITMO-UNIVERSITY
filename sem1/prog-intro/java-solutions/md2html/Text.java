package md2html;

import java.util.*;

public class Text {
    private static final String[] mdKey = new String[]{"*", "_", "**", "__", "--", "`", "''"};
    private static final String[] htmlKey = new String[]{"em", "em", "strong", "strong", "s", "code", "q"};
    private static final Map<String, Integer> keys = new HashMap<>();
    private static final Map<String, String> symbols = Map.of("<", "&lt;", ">", "&gt;", "&", "&amp;");

    static {
        for (int i = 0; i < mdKey.length; i++) {
            keys.put(mdKey[i], i);
        }
    }

    private final StringBuilder input;

    public Text(StringBuilder input) {
        this.input = input;
    }

    public void md2Html(StringBuilder output, int start) {
        //System.err.println(input + " " + start);
        //Stack[] indexes = new Stack[keys.size()];
        List<ArrayList<Integer>> indexes = new ArrayList<>();
        for (int i = 0; i < keys.size(); i++) {
            indexes.add(new ArrayList<>());
        }
        List<Integer> indexesOfSlash = new ArrayList<>();
        List<Integer> indexesOfSymbols = new ArrayList<>();
        for (int i = start; i < input.length(); i++) {
            String last2 = (i == input.length() - 1) ? input.substring(i) : input.substring(i, i + 2);
            if (symbols.containsKey(last2.substring(0, 1))) {
                indexesOfSymbols.add(i);
                continue;
            }
            if (last2.charAt(0) == '\\') {
                indexesOfSlash.add(i);
                i++;
                continue;
            }

            Integer pos = keys.get(last2);
            if (pos == null) {
                pos = keys.get(last2.substring(0, 1));
            }

            //System.err.println(pos);
            if (pos != null) {
                indexes.get(pos).add(i);
                if (mdKey[pos].length() == 2) {
                    i++;
                }
            }
        }
        for (ArrayList<Integer> stack : indexes) {
            if (stack.size() % 2 == 1) {
                stack.remove(stack.size() - 1);
            }
            //System.err.println(stack);
        }
        List<Triple> intervals = new ArrayList<>();
        for (int i = 0; i < indexes.size(); i++) {
            int length = indexes.get(i).size();
            int size = mdKey[i].length();
            for (int j = length - 1; j >= 0; j--) {
                int value1 = indexes.get(i).remove(indexes.get(i).size() - 1);
                intervals.add(new Triple(value1, size, j % 2));
            }
        }
        addToIntervals(indexesOfSlash, intervals, 1, -1);
        addToIntervals(indexesOfSymbols, intervals, 1, -2);
        Collections.sort(intervals);
//         It is possible to sort faster using merge function, but I
//         am to lazy to make it so simple comparator and
//         collection.sort will be here
//        System.err.println(indexesOfSlash);
//        System.err.println("size of intervals: " + intervals.size());
//            for (int i = 0; i < intervals.size(); i++) {
//                System.err.println(intervals.get(i));
//            }
//        System.err.println(leftBorder + " " + rightBorder);
        if (intervals.size() == 0) {
            output.append(input.substring(start));
        } else {
            int leftBorder = intervals.get(0).getValue1();
            int rightBorder;
            output.append(input.substring(start, leftBorder));
            for (int i = 0; i < intervals.size(); i++) {
                Triple triple = intervals.get(i);
                if (triple.getValue3() > -1) {
                    int key = keys.get(input.substring(triple.getValue1(), triple.getValue1() + triple.getValue2()));
                    if (triple.getValue3() == 0) {
                        output.append("<").append(htmlKey[key]).append(">");
                    } else {
                        output.append("</").append(htmlKey[key]).append(">");
                    }
                }
                if (triple.getValue3() == -2) {
                    String substring = input.substring(triple.getValue1(), triple.getValue1() + triple.getValue2());
                    output.append(symbols.get(substring));
                }

                leftBorder = triple.getValue1() + triple.getValue2();
                rightBorder = (i == intervals.size() - 1) ? input.length() : intervals.get(i + 1).getValue1();
                String substring = input.substring(leftBorder, rightBorder);
                output.append(substring);
            }
        }
    }

    private void addToIntervals(List<Integer> stack, List<Triple> intervals, int size, int type) {
        for (int s : stack) {
            intervals.add(new Triple(s, size, type));
        }
    }
}
