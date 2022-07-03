import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;


public class A {
    public static void main(String[] args) {
        try (Scanner sc = new Scanner(System.in)) {
            int a = sc.nextInt();
            int b = sc.nextInt();
            int n = sc.nextInt();
            System.out.println(divideCeil(n - b, b - a) * 2 + 1);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static int divideCeil(int a, int b) {
        if (a % b > 0) {
            return a / b + 1;
        } else {
            return a / b;
        }
    }

    //-----------My Scanner class for faster input---------
    public interface Checker {
        boolean isCharacterValid(char c);
    }

    static class NotWhitespaceChecker implements Checker {
        @Override
        public boolean isCharacterValid(char c) {
            return !(Character.isWhitespace(c));
        }
    }

    public static class Scanner implements AutoCloseable {
        private static final char[] mas = new char[]{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'};
        private final Reader reader;
        private final Checker checker;
        private final char[] buffer = new char[1024];
        char[] lineSeparator = System.lineSeparator().toCharArray();
        char[] miniBuffer = new char[lineSeparator.length];
        private char savedChar;
        private boolean hasSavedChar;
        private boolean hasSavedBuffer;
        private int read;
        private int currentIndex;
        private int miniBufferIndex;

        public Scanner(InputStreamReader reader, Checker checker) {
            this.reader = reader;
            this.checker = checker;
            savedChar = 0;
            hasSavedChar = false;
            hasSavedBuffer = false;
        }

        public Scanner(InputStream input) {
            this(new InputStreamReader(input, StandardCharsets.UTF_8), new NotWhitespaceChecker());
        }

        public Scanner(String input, Charset cs, Checker checker) throws IOException {
            this(new InputStreamReader(new FileInputStream(input), cs), checker);
        }

        public static int hexToDecimal(String hexnum) {
            String hstring = "0123456789ABCDEF";
            hexnum = hexnum.toUpperCase();
            int num = 0;
            for (int i = 0; i < hexnum.length(); i++) {
                char ch = hexnum.charAt(i);
                int n = hstring.indexOf(ch);
                num = 16 * num + n;
            }
            return num;
        }

        @Override
        public void close() throws IOException {
            reader.close();
        }

        private boolean readInput() throws IOException {
            //System.err.println("readInput got called");
            if (!hasSavedBuffer) {
                read = reader.read(buffer);
                //System.err.println(read);
                //System.err.println(Arrays.toString(buffer));
                if (read < 0) {
                    //System.err.println("We reached the end");
                    hasSavedChar = false;
                    return false;
                }
                hasSavedBuffer = true;
                currentIndex = 0;
            }
            if (currentIndex == read - 1) {
                hasSavedBuffer = false;
            }
            savedChar = buffer[currentIndex++];
            miniBuffer[miniBufferIndex] = savedChar;
            miniBufferIndex = (miniBufferIndex + 1) % miniBuffer.length;
            //miniBuffer[currentIndex % 2] = savedChar;
            //System.err.println("saved char: " + savedChar);
            hasSavedChar = true;
            return true;
        }

        private void skipInvalidCharactersExceptSeparator() throws IOException {
            while (hasSavedChar || readInput()) {
                if (!(checker.isCharacterValid(savedChar) || isLineSeparator())) {
                    hasSavedChar = false;
                } else {
                    break;
                }
            }
        }

        private boolean isLineSeparator() {
            int count = 0;
            for (int i = 0; i < miniBuffer.length; i++) {
                if (miniBuffer[(miniBufferIndex + i) % miniBuffer.length] == lineSeparator[i]) {
                    count++;
                }
            }
            return count == miniBuffer.length;
        }

        public boolean hasInput() throws IOException {
            return hasSavedChar || readInput();
        }

        public boolean hasNext() throws IOException {
            skipInvalidCharactersExceptSeparator();
            while (hasInput() && isLineSeparator()) {
                hasSavedChar = false;
                skipInvalidCharactersExceptSeparator();
            }
            return hasSavedChar;
        }

        public String next() throws IOException {
            hasNext();
            StringBuilder sb = new StringBuilder();
            while (hasSavedChar || readInput()) {
                if (checker.isCharacterValid(savedChar)) {
                    sb.append(savedChar);
                    hasSavedChar = false;
                } else {
                    break;
                }
            }
            return sb.toString();
        }

        public String nextHex() throws IOException {
            String next = next();
            return String.valueOf(hexToDecimal(next));
        }

        public String nextAbc() throws IOException {
            String next = next();
            StringBuilder sb = new StringBuilder();
            //char[] mas = new char[]{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'};
            for (int i = 0; i < next.length(); i++) {
                if (!Character.isLetter(next.charAt(i))) {
                    sb.append(next.charAt(i));
                } else {
                    sb.append(Arrays.binarySearch(mas, next.charAt(i)));
                }
            }
            return sb.toString();
        }

        public String nextHexAbc() throws IOException {
            String next = next();
            //System.err.println(next);
            StringBuilder sb = new StringBuilder();
            if (next.startsWith("-")) {
                sb.append("-");
                next = next.substring(1);
            }
            if (next.startsWith("0x") || next.startsWith("0X")) {
                //next = String.valueOf(Integer.parseInt(next.substring(2), 16)); // Беды с парсером :(
                next = toDec(next);
                //System.err.println(next);
                if (next.startsWith("-")) {
                    sb.append("-");
                    //System.err.println(next);
                    next = next.substring(1);
                    //System.err.println(next);
                }
            }
            //System.err.println(next);
            String[] mas = new String[]{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"};
            for (int i = 0; i < next.length(); i++) {
                if (!Character.isLetter(next.charAt(i))) {
                    sb.append(mas[Integer.parseInt(next.substring(i, i + 1))]);
                } else {
                    sb.append(next.charAt(i));
                }
            }
            return sb.toString();
        }

        private String toDec(String hexNumber) {
            char[] hexDigits = new char[]{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
            char[] hexDigitsSmall = new char[]{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
            char[] hexNumberDigits = hexNumber.toCharArray();
            //System.err.println(Arrays.toString(hexNumberDigits));
            if (hexNumberDigits.length == 8) {
                for (int i = 8; i < 16; i++) {
                    if (hexNumberDigits[0] == hexDigits[i] || hexNumberDigits[0] == hexDigitsSmall[i]) {
                        System.err.println(i);
                        for (int j = 0; j < 8; j++) {
                            //System.err.println(Arrays.binarySearch(hexDigits, hexNumberDigits[j]));
                            if (hexNumberDigits[0] == hexDigits[i]) {
                                hexNumberDigits[j] = hexDigits[15 - Arrays.binarySearch(hexDigits, hexNumberDigits[j])];
                                //System.err.println(hexNumberDigits[j]);
                            } else {
                                hexNumberDigits[j] = hexDigits[15 - Arrays.binarySearch(hexDigitsSmall, hexNumberDigits[j])];
                                //System.err.println(hexNumberDigits[j]);
                            }
                        }
                        //System.err.println(Integer.parseInt(String.valueOf(hexNumberDigits), 16));
                        int abs = hexToDecimal(String.valueOf(hexNumberDigits)) + 1;
                        hexNumber = "-" + abs;
                        return hexNumber;
                    }
                }
            }
            //System.err.println(hexNumber);
            return String.valueOf(hexToDecimal(hexNumber.substring(2)));

        }

        public boolean hasCharInLine() throws IOException {
            skipInvalidCharactersExceptSeparator();
            //System.err.println("next char: " + savedChar);
            if (isLineSeparator()) {
                hasSavedChar = false;
                readInput();
                return false;
            }
            return true;
        }

        public int nextInt() throws IOException {
            return Integer.parseInt(next());
        }
    }
}
