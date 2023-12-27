package game;

import java.util.Arrays;
import java.util.Map;

public class TicTacToeBoard implements Board, Position {
    private static final Map<Cell, String> CELL_TO_STRING = Map.of(
            Cell.E, ".",
            Cell.X, "X",
            Cell.O, "0",
            Cell.Y, "-",
            Cell.Z, "|"
    );
    private final int m, n, k, numberOfPlayers;
    private final Cell[][] field;
    private int turn;
    private Cell[] turns = new Cell[]{Cell.X, Cell.O, Cell.Y, Cell.Z};

    public TicTacToeBoard(int m, int n, int k, int numberOfPlayers) {
        this.m = m;
        this.n = n;
        this.k = k;
        this.numberOfPlayers = numberOfPlayers;
        field = new Cell[m][n];
        for (Cell[] row : field) {
            Arrays.fill(row, Cell.E);
        }
        turn = 0;
    }

    @Override
    public Cell getTurn() {
        return turns[turn];
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public GameResult makeMove(Move move) {
        if (!isValid(move)) {
            return GameResult.LOOSE;
        }

        field[move.getRow()][move.getCol()] = move.getValue();
        if (checkWin()) {
            return GameResult.WIN;
        }

        if (checkDraw()) {
            return GameResult.DRAW;
        }

        turn = (turn + 1) % numberOfPlayers;
        return GameResult.UNKNOWN;
    }

    private boolean checkDraw() {
        int count = 0;
        for (int r = 0; r < m; r++) {
            for (int c = 0; c < n; c++) {
                if (field[r][c] == Cell.E) {
                    count++;
                }
            }
        }
        return count == 0;
    }

    private boolean checkWin() {
        for (int r = 0; r < m; r++) {
            int count = 0;
            for (int c = 0; c < n; c++) {
                if (field[r][c] == turns[turn]) {
                    count++;
                } else {
                    count = 0;
                }
                if (count == k) {
                    return true;
                }
            }
        }
        for (int c = 0; c < n; c++) {
            int count = 0;
            for (int r = 0; r < m; r++) {
                if (field[r][c] == turns[turn]) {
                    count++;
                } else {
                    count = 0;
                }
                if (count == k) {
                    return true;
                }
            }
        }

        for (int r = 0; r <= m - k; r++) {
            for (int c = 0; c <= n - k; c++) {
                int count = 0;
                for (int i = 0; i < k; i++) {
                    if (field[r + i][c + i] == turns[turn]) {
                        count++;
                    }
                }
                if (count == k) {
                    return true;
                }
            }
        }
        for (int r = 0; r <= m - k; r++) {
            for (int c = k - 1; c < n; c++) {
                int count = 0;
                for (int i = 0; i < k; i++) {
                    if (field[r + i][c - i] == turns[turn]) {
                        count++;
                    }
                }
                if (count == k) {
                    return true;
                }
            }
        }
        return false;
    }

    public boolean isValid(final Move move) {
        return 0 <= move.getRow() && move.getRow() < m
                && 0 <= move.getCol() && move.getCol() < n
                && field[move.getRow()][move.getCol()] == Cell.E
                && turns[turn] == move.getValue();
    }

    @Override
    public Cell getCell(int row, int column) {
        return field[row][column];
    }

    @Override
    public int getRowSize() {
        return m;
    }

    @Override
    public int getColSize() {
        return n;
    }

    @Override
    public String toString() {
        int blockSize = String.valueOf(Math.max(m, n)).length() + 1;
        //Format that shit
        final StringBuilder sb = new StringBuilder();
        format(blockSize, sb, "");
        for (int c = 1; c <= n; c++) {
            format(blockSize, sb, String.valueOf(c));
        }
        sb.append(System.lineSeparator());
        for (int r = 0; r < m; r++) {
            format(blockSize, sb, String.valueOf(r + 1));
            for (Cell cell : field[r]) {
                format(blockSize, sb, CELL_TO_STRING.get(cell));
            }
            sb.append(System.lineSeparator());
        }
        sb.setLength(sb.length() - System.lineSeparator().length());
        return sb.toString();
    }

    private void format(int blockSize, StringBuilder sb, String c) {
        sb.append(c);
        sb.append(" ".repeat(Math.max(0, blockSize - c.length())));
    }
}


