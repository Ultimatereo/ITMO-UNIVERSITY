import java.util.*;
import java.io.*;

class WordStatIndexChecker implements Checker {
    public boolean isWordCharacter(char c) {
        return Character.getType(c) == Character.DASH_PUNCTUATION
                || c == '\''
                || Character.isLetter(c);
    }
}
public class WordStatSortedLineIndex {
	private static Checker wordStatIndexChecker = new WordStatIndexChecker();
	public static void main (String[] args) {
		String inputName = args[0];
		String outputName = args[1];
		SortedMap<String, Integer> wordsNumbers = new TreeMap<>();
		SortedMap<String, String> wordsPlaces = new TreeMap<>();
		int nS = 0;
		int nInS = 0;
		int j;
		String currentWord;
		try {
			Scanner reader = new Scanner(new BufferedReader(new InputStreamReader(
				new FileInputStream(inputName), "UTF-8")), wordStatIndexChecker);
			try {
				while (!reader.isEmpty()) {
					nS += 1;
					nInS = 0;
					while (!reader.isEndOfLine()) {
						nInS += 1;
						currentWord = reader.next().toLowerCase();
						wordsNumbers.put(currentWord, wordsNumbers.getOrDefault(currentWord, 0) + 1);
						wordsPlaces.put(currentWord, wordsPlaces.getOrDefault(currentWord, "") + " " + nS + ":" + nInS);
					}
					reader.skipAllLine();
				}
			} finally {
				reader.close();
			}
		} catch (IOException e) {
			System.out.println("I/O error: " + e.getMessage());
		}
		Iterator iterator = wordsNumbers.keySet().iterator();

		try {
			BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(outputName), "UTF-8"));
			try {
				while(iterator.hasNext()) {
					String word = (String) iterator.next();
					writer.write(word + " " + wordsNumbers.get(word) + wordsPlaces.get(word));
					writer.newLine();
				}
			} finally {
				writer.close();
			}
		} catch (IOException e) {
			System.out.println("I/O error: " + e.getMessage());
		}


	}
}