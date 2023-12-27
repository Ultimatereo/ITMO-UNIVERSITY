import java.util.*;
import java.io.*;
 
public class zad4 {
	private interface Checker {
	    boolean isWordCharacter(char c);
	}
	public static int pow(int x, int y) { 
		int res = x;
		for (int i = 1; i < y; i++) {
			res = res * x;
		}
		return res;
	}
	public static ArrayList<String> chainCode(int n) {
		String current = "0".repeat(n);
		ArrayList<String> result = new ArrayList<>();
		result.add(current);
		String prefix;
		while (true) {
			prefix = current.substring(1);
			if (!result.contains(prefix + "1")) {
				current = prefix + "1";
			} else if (!result.contains(prefix + "0")) {
				current = prefix + "0";
			} else {
				break;
			}
			result.add(current);
		}
		return result;
	}
	public static void main (String[] args) throws IOException{
		class Scanner implements AutoCloseable {
		    private Reader reader;
		    private char savedChar;
		    private boolean hasSaved;
		    private Checker checker;
 
		    public Scanner(Reader r, Checker checker) {
		        this.checker = checker;
		        reader = r;
		        hasSaved = false;
		        savedChar = 0;
		    }
 
		    public Scanner(InputStream in, Checker checker) {
		        this(new BufferedReader(new InputStreamReader(in)), checker);
		    }
 
		    public void close() throws IOException {
		        reader.close();
		    }
 
		    private boolean readInput() throws IOException {
		        if (!hasSaved) {
		            int res = reader.read();
		            if (res < 0) {
		                hasSaved = false;
		                return false;
		            }
		            savedChar = (char) res;
		            hasSaved = true;
		        }
		        return true;
		    }
 
		    private boolean hasInput() throws IOException {
		        return (hasSaved) || readInput();
		    }
 
		    private void skipWhitespacesExceptSeparator(Checker checker) throws IOException {
		        while (hasInput() && !checker.isWordCharacter(savedChar) && (savedChar != '\n')) {
		            hasSaved = false;
		        }
		    }
 
		    public void skipAllLine() throws IOException {
		        while (hasInput()) {
		            if (savedChar == '\n') {
		                hasSaved = false;
		                readInput();
		                break;
		            }
		            hasSaved = false;
		        }
		    }
 
		    public boolean isEndOfLine() throws IOException {
		        skipWhitespacesExceptSeparator(checker);
		        return !hasInput() || (savedChar == '\n');
		    }
 
		    public String next() throws IOException {
		        skipWhitespacesExceptSeparator(checker);
		        StringBuilder result = new StringBuilder();
		        while (hasInput() && checker.isWordCharacter(savedChar)) {
		            result.append(savedChar);
		            hasSaved = false;
		        }
		        return result.toString();
		    }
 
		    public int nextInt() throws IOException {
		        return Integer.parseInt(next());
		    }

		    public boolean isEmpty() throws IOException {
		        skipWhitespacesExceptSeparator(checker);
		        return !hasInput();
		    }
		}
		class ReverseChecker implements Checker {
		    public boolean isWordCharacter(char c) {
		        return !Character.isWhitespace(c);
		    }
		}
		ReverseChecker reverseChecker = new ReverseChecker();
		Scanner input = new Scanner(new BufferedReader(new InputStreamReader(
				new FileInputStream("testin.txt"), "UTF-8")), wordStatIndexChecker);
		PrintWriter output = new PrintWriter("testout.txt");
		int n = input.nextInt();
		input.close();
		ArrayList<String> result = chainCode(n);
		for (int i = 0; i < result.size(); i++) {
			output.write(result.get(i));
			output.write('\n');
		}
	}
}