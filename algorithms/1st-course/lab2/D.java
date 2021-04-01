import java.util.*;
import java.io.*;
 
 
public class D {
	private interface Checker {
	    boolean isWordCharacter(char c);
	}
	private static int[] expandArraySize(int[] arr) {
	    int[] answer = new int[arr.length * 2];
	    for (int i = 0; i < arr.length; i++) {
	    	answer[i] = arr[i];
	    }
	    return answer;
	} 
	private static int[] broadenArraySize(int[] arr) {
	    int[] answer = new int[arr.length / 2];
	    for (int i = 0; i < arr.length / 2; i++) {
	    	answer[i] = arr[i];
	    }
	    return answer;
	} 
	private static int min (int[] arr, int size) {
	    int min = Integer.MAX_VALUE;
	    for (int i = 0; i < size; i++) {
	    	if (arr[i] < min) {
	    		min = arr[i];
	    	}
	    }
	    return min;
	}
	private static int indexOf (int[] arr, int ind) {
		int answer = 0;
	    for (int i = 0; i < arr.length; i++) {
	    	if (arr[i] == ind) {
	    		answer = i;
	    		break;
	    	}
	    }
	    return answer;
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
 
		}
		class ReverseChecker implements Checker {
		    public boolean isWordCharacter(char c) {
		        return !Character.isWhitespace(c);
		    }
		}
		ReverseChecker reverseChecker = new ReverseChecker();
		Scanner input = new Scanner(System.in, reverseChecker);
		PrintWriter output = new PrintWriter(System.out);
		int n = input.nextInt();
		input.skipAllLine();
		ArrayDeque<Integer> first = new ArrayDeque<>();
		ArrayDeque<Integer> second = new ArrayDeque<>();
		String key;
		int x;
		for (int i = 0; i < n; i++) {
			key = input.next();
			if (key.equals("-")) {
				output.write(String.valueOf(first.pollFirst()));
				output.write('\n');
			} else if (key.equals("+")) {
				second.addLast(input.nextInt());
			} else {
				second.addFirst(input.nextInt());
			}
			if (second.size() > first.size()) {
				first.addLast(second.pollFirst());
			}
			input.skipAllLine();
		}
		input.close();
		output.close();
	}
}
