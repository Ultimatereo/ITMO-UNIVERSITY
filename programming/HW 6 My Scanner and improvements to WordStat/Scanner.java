import java.io.*;

public class Scanner implements AutoCloseable {
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

    public boolean isEmpty() throws IOException {
        skipWhitespacesExceptSeparator(checker);
        return !hasInput();
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

    public boolean hasNext() throws IOException {
        while (hasInput() && !checker.isWordCharacter(savedChar)) {
            hasSaved = false;
        }
        return hasInput();
    }

    public int nextInt() throws IOException {
        return Integer.parseInt(next());
    }

}