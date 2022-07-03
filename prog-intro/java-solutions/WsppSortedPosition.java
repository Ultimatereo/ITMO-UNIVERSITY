import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class WsppSortedPosition {
    public static void main(String[] args) {
        String input = args[0];
        String output = args[1];
        Map<String, List<Pair>> wordsAndCounters = new TreeMap<>();
        String lineSeparator = System.lineSeparator();
        int counter;
        int stringCounter = 1;
        try {
            try (Scanner scanner = new Scanner(input, StandardCharsets.UTF_8, new WordChecker())) {
                while (scanner.hasNext()) {
                    counter = 1;
                    while (scanner.hasCharInLine()) {
                        String word = scanner.next().toLowerCase();
                        List<Pair> defaultArray = wordsAndCounters.getOrDefault(word, new ArrayList<>());
                        defaultArray.add(new Pair(stringCounter, counter));
                        wordsAndCounters.put(word, defaultArray);
                        counter++;
                    }
                    stringCounter++;
                }
            }
            try (BufferedWriter writer = new BufferedWriter(
                    new OutputStreamWriter(new FileOutputStream(output), StandardCharsets.UTF_8)
                    // We can use ARM instead of try + finally block since java 1.7 which is very useful
            )) {
                for (String key : wordsAndCounters.keySet()) {
                    writer.write(key + " " + wordsAndCounters.get(key).size());
                    for (Pair pair : wordsAndCounters.get(key)) {
                        //System.err.println(pair);
                        writer.write(" " + pair.toString());
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
