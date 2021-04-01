import java.util.*;
import java.io.*;

class WordStatIndexChecker implements Checker {
    public boolean isWordCharacter(char c) {
        return Character.getType(c) == Character.DASH_PUNCTUATION
                || c == '\''
                || Character.isLetter(c);
    }
}
public class WordStatLineIndex {
	private static Checker wordStatIndexChecker = new WordStatIndexChecker();
	private static int[] expandArraySize(int[] arr) {
        return Arrays.copyOf(arr, arr.length * 2);
    }
    private static String[] expandArraySizeString(String[] arr) {
        return Arrays.copyOf(arr, arr.length * 2);
    }
    /*
    private static classNumbers {
    	IntList numbersArray;
    	int numbersSize;
    	numbers() {
    		numbersArray = new IntList
    	}
    }
    */
	public static void main (String[] args) {
		String inputName = args[0];
		String outputName = args[1];
		ArrayList<String> words = new ArrayList<>();
		ArrayList<String> places = new ArrayList<>();
		IntList numbers = new IntList();
		int nS = 0;
		int nInS = 0;
		int j;
		String currentWord;
		try {
			Scanner reader = new Scanner(new BufferedReader(new InputStreamReader(
				new FileInputStream(inputName), "UTF-8")), wordStatIndexChecker);
			while (!reader.isEmpty()) {
				nS += 1;
				nInS = 0;
				while (!reader.isEndOfLine()) {
					nInS += 1;
					currentWord = reader.next().toLowerCase();
					j = words.indexOf(currentWord);
					if (j == -1) {
						words.add(currentWord);
						numbers.add(1);
						places.add(String.valueOf(nS) + ":" + String.valueOf(nInS));
					} else {
						numbers.set(j, numbers.get(j) + 1);
						places.set(j, (places.get(j) + " " + String.valueOf(nS) + ":" + String.valueOf(nInS)));
					}
				}
				reader.skipAllLine();
			}
			reader.close();
		} catch (IOException e) {
			System.out.println("I/O error: " + e.getMessage());
		}
		try {
			PrintWriter writer = new PrintWriter(new OutputStreamWriter(
				new FileOutputStream(outputName), "UTF-8"));
			try {
				for (int i = 0; i < words.size(); i++) {
					writer.println(words.get(i) + " " + numbers.get(i) + " " + places.get(i));
				}
			} finally {
				writer.close();
			}
		} catch (IOException e) {
			System.out.println("I/O error: " + e.getMessage());
		}


	}
}