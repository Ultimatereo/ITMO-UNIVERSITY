package game;

public class Table {

    private int playerNo;
    private int[] table;

    public Table(int playerNo) {
        this.playerNo = playerNo;
        table = new int[playerNo];
    }


    public void add(int pos, int cur) {
        table[pos] += cur;
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder();
        for (int r = 1; r <= playerNo; r++) {
            sb.append("Player " + r + '\n');
            sb.append(table[r - 1]);
            sb.append('\n');
        }
        return sb.toString();
    }
}