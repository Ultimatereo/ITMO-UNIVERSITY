import java.io.*;
import java.nio.charset.StandardCharsets;

public class WordStatCount {
    public static void main(String[] args) {
        String input = args[0];
        String output = args[1];
        //StringBuilder word = new StringBuilder();
        String[] words = new String[64];
        int wordsCounter = 0;
        try {
            try (Scanner scanner = new Scanner(input, StandardCharsets.UTF_8, new WordChecker())) {
                while (scanner.hasNext()) {
                    String word = scanner.next();
                    //System.err.println("word: " + word);
                    words = Functions.add(words, word.toLowerCase(), wordsCounter++);

//                    char c = (char) character;
//                    if (Character.isLetter(c) ||
//                            Character.getType(c) == Character.DASH_PUNCTUATION ||
//                            c == '\'') {
//                        word.append(c);
//                    } else {
//                        if (word.length() > 0) {
//                            //System.err.println(word);
//                            // Here put them in your arrays
//                            words = Functions.add(words, word.toString().toLowerCase(), wordsCounter++);
//                            //System.err.println(Arrays.toString(words));
//                            //System.err.println(Arrays.toString(counters));
//                            word = new StringBuilder();
//                        }
//                    }
                }
            }
            //System.err.println(Arrays.toString(words));
            String[] notSortedWords = new String[wordsCounter];
            System.arraycopy(words, 0, notSortedWords, 0, wordsCounter);
            int[] originalIndexes = new int[wordsCounter];
            for (int i = 0; i < wordsCounter; i++) {
                originalIndexes[i] = i;
            }
            Functions.heapSort(originalIndexes, words, wordsCounter);
//            for (int i = 0; i < wordsCounter; i++) {
//                System.err.println(originalIndexes[i] + " " + notSortedWords[originalIndexes[i]]);
//            }
//            System.err.println(Arrays.toString(originalIndexes));
            int[] counters = new int[64];
            int[] uniqueIndexes = new int[64];
            int uniqueIndex;
            int uniqueWordsCounter = 0;
            int k = 0;
            int counter;
            while (k < wordsCounter) {
                counter = 1;
                uniqueIndex = k;
                while (k != wordsCounter - 1 && words[k].equals(words[k + 1])) {
                    k++;
                    counter++;
                }
                uniqueIndexes = Functions.add(uniqueIndexes, originalIndexes[uniqueIndex], uniqueWordsCounter);
                counters = Functions.add(counters, counter, uniqueWordsCounter++);
                k++;
            }
//            System.err.println(Arrays.toString(uniqueIndexes));
//            System.err.println(Arrays.toString(counters));
//            for (int i = 0; i < uniqueWordsCounter; i++) {
//                System.err.println(notSortedWords[uniqueIndexes[i]] + " " + counters[i]);
//            }
//            Functions.heapSort(uniqueIndexes, counters, uniqueWordsCounter);
//            System.err.println(Arrays.toString(uniqueIndexes));
//            System.err.println(Arrays.toString(counters));
            Functions.heapSort(uniqueIndexes, counters, uniqueWordsCounter);
            try (BufferedWriter writer = new BufferedWriter(
                    new OutputStreamWriter(new FileOutputStream(output), StandardCharsets.UTF_8)
                    // We can use ARM instead of try + finally block since java 1.7 which is very useful
            )) {
                // Here write them
                for (int i = 0; i < uniqueWordsCounter; i++) {
                    writer.write(notSortedWords[uniqueIndexes[i]] + " " + counters[i] + "\n");
                }
            }
        } catch (FileNotFoundException e) {
            System.err.println("File not found!");
            e.printStackTrace();
        } catch (IOException e) {
            System.err.println("I/O error occurred!");
            e.printStackTrace();
        }
    }
}
