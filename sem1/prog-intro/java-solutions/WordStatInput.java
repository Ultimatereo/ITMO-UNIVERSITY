import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;
public class WordStatInput {
    public static int[] add(int[] arr, int x, int i) {
        if (i == arr.length) {
            int[] newArr = new int[arr.length * 2];
            System.arraycopy(arr, 0, newArr, 0, arr.length);
            newArr[arr.length] = x;
            return newArr;
        }

        arr[i] = x;
        return arr;
    }
    public static String[] add(String[] arr, String x, int i) {
        if (i == arr.length) {
            String[] newArr = new String[arr.length * 2];
            System.arraycopy(arr, 0, newArr, 0, arr.length);
            newArr[arr.length] = x;
            return newArr;
        }

        arr[i] = x;
        return arr;
    }

    public static void main(String[] args) {
        String input = args[0];
        String output = args[1];
        StringBuilder word = new StringBuilder();
        String[] words = new String[1024];
        int wordsCounter = 0;
        try {
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(new FileInputStream(input), StandardCharsets.UTF_8)
                    // We can use ARM instead of try + finally block since java 1.7 which is very useful
            )) {
                char[] block = new char[1024];
                while (true) {
                    int read = reader.read(block);
                    if (read == -1) {
                        //System.err.println("read() returned -1 that means EOF");
                        break;
                    }
                    for (int i = 0; i < read; i++) {
                        if (Character.isLetter(block[i]) ||
                                Character.getType(block[i]) == Character.DASH_PUNCTUATION ||
                                block[i] == '\'') {
                            word.append(block[i]);
                        } else {
                            if (word.length() > 0) {
                                //System.err.println(word);
                                // Here put them in your arrays
                                words = add(words, word.toString().toLowerCase(), wordsCounter++);
                                //System.err.println(Arrays.toString(words));
                                //System.err.println(Arrays.toString(counters));
                                word = new StringBuilder();
                            }
                        }
                    }
                }
            }
            //System.err.println(Arrays.toString(words));
            String[] notSortedWords = new String[wordsCounter];
            System.arraycopy(words, 0, notSortedWords, 0, wordsCounter);
            String[] sortedWords = Functions.quickSort(words, 0, wordsCounter - 1);
            //String[] sortedWords = functions.heapSort(words, wordsCounter);
            int[] counters = new int[1024];
            String[] uniqueWords = new String[1024];
            int uniqueWordsCounter = 0;
            int k = 0;
            int counter;
            while (k < wordsCounter) {
                counter = 1;
                while (sortedWords[k].equals(sortedWords[k + 1])) {
                    k++;
                    counter++;
                }
                uniqueWords = add(uniqueWords, sortedWords[k], uniqueWordsCounter);
                counters = add(counters, counter, uniqueWordsCounter++);
                k++;
            }
//            System.err.println(Arrays.toString(sortedWords));
//            System.err.println(Arrays.toString(uniqueWords));
//            System.err.println(Arrays.toString(counters));
            boolean[] itWasBefore = new boolean[uniqueWordsCounter];
            int index;
            try (BufferedWriter writer = new BufferedWriter(
                    new OutputStreamWriter(new FileOutputStream(output), StandardCharsets.UTF_8)
                    // We can use ARM instead of try + finally block since java 1.7 which is very useful
            )) {
                // Here write them
//                for (int i = 0; i < counterUniqueWords; i++) {
//                    // В лексикографическом порядке:
//                    writer.write(uniqueWords[i] + " " + counters[i] + "\n");
//                    }
//                System.err.println(Arrays.toString(notSortedWords));
                for (int i = 0; i < wordsCounter; i++) {
                    index = Functions.binarySearch(uniqueWords, 0, uniqueWordsCounter - 1, notSortedWords[i]);
//                    System.err.println(index);
                    if (!itWasBefore[index]) {
                        writer.write(notSortedWords[i] + " " + counters[index] + "\n");
                        itWasBefore[index] = true;
                    }
                }
            }
        } catch (ArrayIndexOutOfBoundsException e) {
            System.err.println("Not enough arguments: " + e.getMessage());
            e.printStackTrace();
        } catch (FileNotFoundException e){
            System.err.println("File not found: " + e.getMessage());
            e.printStackTrace();
        } catch (IOException e) {
            System.err.println("I/O error occurred: " + e.getMessage());
            e.printStackTrace();
        }
    }


}
