import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;

//class WordChecker implements Checker {
//    @Override
//    public boolean isCharacterValid(char c) {
//        return Character.isLetter(c) ||
//                Character.getType(c) == Character.DASH_PUNCTUATION ||
//                c == '\'';
//    }
//}

public class Wspp {
    public static void main(String[] args) {
        String input = args[0];
        String output = args[1];
        Map<String, ArrayList<Integer>> wordsAndCounters = new LinkedHashMap<>();
        String lineSeparator = System.lineSeparator();
        int counter = 1;
        try {
            try (Scanner scanner = new Scanner(input, StandardCharsets.UTF_8, new WordChecker())) {
                while (scanner.hasNext()) {
                    String word = scanner.next().toLowerCase();
                    if (wordsAndCounters.containsKey(word)) {
                        wordsAndCounters.get(word).set(0, wordsAndCounters.get(word).get(0) + 1);
                    } else {
                        ArrayList<Integer> defaultArray = new ArrayList<>();
                        defaultArray.add(1);
                        wordsAndCounters.put(word, defaultArray);
                    }
                    wordsAndCounters.get(word).add(counter++);
                }
            }
            try (BufferedWriter writer = new BufferedWriter(
                    new OutputStreamWriter(new FileOutputStream(output), StandardCharsets.UTF_8)
                    // We can use ARM instead of try + finally block since java 1.7 which is very useful
            )) {
                for (String key : wordsAndCounters.keySet()) {
                    writer.write(key);
                    for (Integer i : wordsAndCounters.get(key)) {
                        writer.write(" " + i);
                    }
                    writer.write(lineSeparator);
                }
            }
        } catch (IOException e) {
            System.err.println("Uhhh something bad happened: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
