import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;

public class Painter {
    public static void main(String[] args) {
        try (Scanner sc = new Scanner(System.in)) {
//            int length = 16;
            //int length = 1_048_576;
            //SegmentTree tree = new SegmentTree(length);
            int n = sc.nextInt();
            String[] commands = new String[n];
            int[] x = new int[n];
            int[] l = new int[n];
            int leftBorder = Integer.MAX_VALUE;
            int rightBorder = Integer.MIN_VALUE;
            for (int i = 0; i < n; i++) {
                commands[i] = sc.next();
                x[i] = sc.nextInt();
                l[i] = sc.nextInt();
//                if (l[i] < 0) {
//                    x[i] = x[i] + l[i] + 1;
//                    l[i] = -l[i];
//                    //l[i] = 0;
//                }
                if (l[i] == 0) {
                    x[i] = Integer.MIN_VALUE;
                } else {
                    rightBorder = Math.max(rightBorder, x[i] + l[i] - 1);
                    leftBorder = Math.min(leftBorder, x[i]);
                }
            }
            int length = findClose2(rightBorder - leftBorder + 1);
            //System.err.println(length);
            SegmentTree tree = new SegmentTree(length);
            int delta = -leftBorder;
            for (int i = 0; i < n; i++) {
                if (l[i] != 0) {
                    if (commands[i].equals("W")) {
                        tree.update(x[i] + delta, x[i] + l[i] - 1 + delta, 0, 0);
                    } else {
                        tree.update(x[i] + delta, x[i] + l[i] - 1 + delta, 0, 1);
                    }
                }
                System.out.println(tree.numOfBlackSegments[0] + " " + tree.numOfBlacks[0]);
            }
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

    //-----------MyScanner class for faster input---------
    public interface Checker {
        boolean isCharacterValid(char c);
    }

    public static class SegmentTree {
        final int length;
        final int[] l;
        final int[] r;
        final int[] numOfBlacks;
        final int[] numOfBlackSegments;
        final boolean[] isModified;
        final int[] set;

        public SegmentTree(int n) {
            this.length = n;
            this.l = new int[2 * n - 1];
            this.r = new int[2 * n - 1];
            this.numOfBlacks = new int[2 * n - 1];
            this.numOfBlackSegments = new int[2 * n - 1];
            this.set = new int[2 * n - 1];
            this.isModified = new boolean[2 * n - 1];
            //this.nodes = new Node[2*n - 1];
            build(0, 0, length - 1);
        }

        private void build(int v, int tl, int tr) {
            if (tl == tr) {
                setNode(v, tl, tr, 0, 0, false, 0);
                return;
            }
            int tm = tl + (tr - tl) / 2;
            build(v * 2 + 1, tl, tm);
            build(v * 2 + 2, tm + 1, tr);
            setNode(v, tl, tr, 0, 0, false, 0);
        }

        private void setNode(int v, int tl, int tr, int numOf_Blacks, int numOf_BlackSegments, boolean is_Modified, int set_) {
            l[v] = tl;
            r[v] = tr;
            numOfBlacks[v] = numOf_Blacks;
            numOfBlackSegments[v] = numOf_BlackSegments;
            isModified[v] = is_Modified;
            set[v] = set_;
        }

        private void propogate(int v) {
            if (!isModified[v]) {
                return;
            }
            isModified[v] = false;

            if (l[v] == r[v]) {
                return;
            }

            set[v * 2 + 1] = set[v];
            set[v * 2 + 2] = set[v];
            isModified[v * 2 + 1] = true;
            isModified[v * 2 + 2] = true;
            numOfBlacks[v * 2 + 1] = set[v * 2 + 1] * (r[v * 2 + 1] - l[v * 2 + 1] + 1);
            numOfBlackSegments[v * 2 + 1] = set[v * 2 + 1];
            numOfBlacks[v * 2 + 2] = set[v * 2 + 2] * (r[v * 2 + 2] - l[v * 2 + 2] + 1);
            numOfBlackSegments[v * 2 + 2] = set[v * 2 + 2];
        }

        public void update(int l, int r, int v, int colour) {
            if ((l > this.r[v]) || (r < this.l[v])) {
                return;
            }
            if ((l <= this.l[v]) && (r >= this.r[v])) {
                isModified[v] = true;
                set[v] = colour;
                numOfBlacks[v] = set[v] * (this.r[v] - this.l[v] + 1);
                numOfBlackSegments[v] = set[v];
                return;
            }
            propogate(v);
            update(l, r, 2 * v + 1, colour);
            update(l, r, 2 * v + 2, colour);

            boolean left = isRightBlack(v * 2 + 1);
            boolean right = isLeftBlack(v * 2 + 2);
            //System.err.println(colour + " " + l + " " + r + " "  + this.l[v] + " " + this.r[v] + " " + left + " " + right);
            numOfBlacks[v] = numOfBlacks[2 * v + 1] + numOfBlacks[2 * v + 2];
            numOfBlackSegments[v] = numOfBlackSegments[2 * v + 1] + numOfBlackSegments[2 * v + 2];
            if (left && right) {
                numOfBlackSegments[v]--;
            }
        }

        private boolean isRightBlack(int v) {
            if (l[v] == r[v] || (isModified[v])) {
                return set[v] == 1;
            }
            return isRightBlack(v * 2 + 2);
        }

        private boolean isLeftBlack(int v) {
            if (l[v] == r[v] || (isModified[v])) {
                return set[v] == 1;
            }
            return isLeftBlack(v * 2 + 1);
        }
    }

    static class NotWhitespaceChecker implements Checker {
        @Override
        public boolean isCharacterValid(char c) {
            return !(Character.isWhitespace(c));
        }
    }

    public static class Scanner implements AutoCloseable {
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

        public int nextInt() throws IOException {
            return Integer.parseInt(next());
        }

    }
}
