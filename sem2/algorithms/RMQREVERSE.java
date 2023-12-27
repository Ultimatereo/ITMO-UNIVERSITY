import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.*;


public class RMQREVERSE {
    public static void main(String[] args) {
        try (Scanner sc = new Scanner(new FileInputStream("rmq.in"))) {
            PrintWriter wr = new PrintWriter(new FileOutputStream("rmq.out"));
            int n = sc.nextInt();
            int m = sc.nextInt();
            Set<Triple> triples = new TreeSet<>();
            for (int i = 0; i < m; i++) {
                triples.add(new Triple(sc.nextInt() - 1, sc.nextInt() - 1, sc.nextInt()));
            }
//            wr.println(n + " " + m);
//            Iterator<Triple> itr = triples.iterator();
//            while (itr.hasNext()) {
//                wr.println(itr.next());
//            }
            long[] mas = new long[n];
            for (int i = 0; i < n; i++) {
                mas[i] = Integer.MAX_VALUE;
            }
            int length = findClose2(n);
            SegmentTree tree = new SegmentTree(length, mas);
            Iterator<Triple> itr = triples.iterator();
            while (itr.hasNext()) {
                Triple triple = itr.next();
                //System.err.println(triple);
                tree.set(triple.l, triple.r, 0, 0, length - 1, triple.value);
                //System.err.println(tree);
            }
            itr = triples.iterator();
            //System.out.println(tree);
            boolean fail = false;
            while (itr.hasNext()) {
                Triple triple = itr.next();
                if (tree.min(triple.l, triple.r, 0, 0, length - 1) != triple.value) {
                    //System.err.println(triple);
                    fail = true;
                    break;
                }
            }
            if (fail) {
                wr.println("inconsistent");
            } else {
                wr.println("consistent");
                for (int i = 0; i < n; i++) {
                    wr.print(tree.min(i, i, 0, 0, length - 1) + " ");
                }
            }
            wr.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    private static int findClose2(int value) {
        int answer = 2;
        while (true) {
            if (answer >= value) {
                return answer;
            }
            answer *= 2;
        }
    }
    public static class SegmentTree {
        private final int length;
        private final long[] mas;
        private final boolean[] isAdded;
        private final boolean[] isSet;
        private final long[] set;

        @Override
        public String toString() {
            return "SegmentTree{" +
                    "length=" + length +
                    ", mas=" + Arrays.toString(mas) +
                    ", isAdded=" + Arrays.toString(isAdded) +
                    ", isSet=" + Arrays.toString(isSet) +
                    ", set=" + Arrays.toString(set) +
                    '}';
        }

        public SegmentTree(int length, long[] mas) {
            this.length = length;
            this.mas = new long[length*2 - 1];
            isAdded = new boolean[length*2 - 1];
            isSet = new boolean[length*2 - 1];
            set = new long[2*length - 1];
            for (int i = 0; i < mas.length; i++) {
                this.mas[length - 1 + i] = mas[i];
            }
            for (int i = mas.length; i < length; i++) {
                this.mas[length - 1 + i] = Long.MAX_VALUE;
            }
            for (int i = length - 2; i >= 0; i--) {
                this.mas[i] = Math.min(this.mas[2*i + 1], this.mas[2*i + 2]);
            }
            //System.err.println(Arrays.toString(this.mas));
        }


        public long min(int l, int r, int v, int tl, int tr) {
            propagate(v);
            if ((l > tr) || (r < tl)) {
                return Long.MAX_VALUE;
            }
            if ((l <= tl) && (tr <= r)) {
                return mas[v];
            }
            int tm = tl + (tr - tl)/2;
            return Math.min(min(l, r, 2*v + 1, tl, tm), min(l, r, 2*v + 2, tm + 1, tr));
        }

        private void propagate(int v) {
            if (isAdded[v]) {
                isAdded[v] = false;
                if (v > length - 2) {
                    return;
                }
                checkChild(v, 2*v + 1);
                checkChild(v, 2*v + 2);
            } else if (isSet[v]) {
                isSet[v] = false;
                if (v > length - 2) {
                    return;
                }
                checkChild2(v, 2*v + 1);
                checkChild2(v, 2*v + 2);
            }
        }
        void checkChild2(int v, int child) {
            if (mas[child] != Long.MAX_VALUE) {
                if (isAdded[child]) {
                    isAdded[child] = false;
                    isSet[child] = true;
                    set[child] = set[v];
                    mas[child] = set[child];
                } else {
                    isSet[child] = true;
                    set[child] = set[v];
                    mas[child] = set[2 * v + 1];
                }
            }
        }

        void checkChild(int v, int child) {
            if (mas[child] != Long.MAX_VALUE) {
                if (isAdded[child]) {
                    set[child] = set[child] + set[v];
                    mas[child] = mas[child] + set[v];
                } else if (isSet[child]) {
                    set[child] = set[child] + set[v];
                    mas[child] = set[child];
                } else {
                    isAdded[child] = true;
                    set[child] = set[v];
                    mas[child] = mas[child] + set[child];
                }
            }
        }
        public void set(int l, int r, int v, int tl, int tr, long value) {
            propagate(v);
            if ((l > tr) || (r < tl)) {
                return;
            }
            if ((l <= tl) && (tr <= r)) {
                mas[v] = value;
                set[v] = value;
                isSet[v] = true;
                return;
            }
            int tm = tl + (tr - tl)/2;
            set(l, r, 2*v + 1, tl, tm, value);
            set(l, r, 2*v + 2, tm + 1, tr, value);
            mas[v] = Math.min(mas[2*v + 1], mas[2*v + 2]);
        }

        public void add(int l, int r, int v, int tl, int tr, long value) {
            propagate(v);
            if ((l > tr) || (r < tl)) {
                return;
            }
            if ((l <= tl) && (tr <= r)) {
                mas[v] = mas[v] + value;
                set[v] = value;
                isAdded[v] = true;
                return;
            }
            int tm = tl + (tr - tl)/2;
            add(l, r, 2*v + 1, tl, tm, value);
            add(l, r, 2*v + 2, tm + 1, tr, value);
            mas[v] = Math.min(mas[2*v + 1], mas[2*v + 2]);
        }
    }
    public static class Triple implements Comparable{

        private final int l;
        private final int r;
        private final int value;

        @Override
        public String toString() {
            return l + " " + r + " " + value;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Triple triple = (Triple) o;
            return l == triple.l && r == triple.r && value == triple.value;
        }

        @Override
        public int hashCode() {
            return Objects.hash(l, r, value);
        }

        public Triple(int l, int r, int value) {
            this.l = l;
            this.r = r;
            this.value = value;
        }

        public int getL() { return l; }
        public int getR() { return r; }
        public int getValue() { return value; }

        @Override
        public int compareTo(Object o) {
            Triple triple = (Triple) o;
            if (l == triple.l && r == triple.r && value == triple.value) {
                return 0;
            }
            if (value < triple.value) {
                return -1;
            }
            return 1;
        }
    }
    //-----------MyScanner class for faster input---------
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
            if (hasNext()) {
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
            return null;
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

        public long nextLong() throws IOException {
            return Long.parseLong(next());
        }
    }
}
