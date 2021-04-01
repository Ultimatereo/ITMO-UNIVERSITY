package game;

import java.util.Arrays;

public class Board implements Position{
    private final int m, n, k;
    private final Cell[][] cells;
    private Cell turn;
    private int empty;

    public Board(int n, int m, int k) {
        this.n = n;
        this.m = m;
        this.k = k;

        empty = n * m;
        cells = new Cell[n][m];
        for (int r = 0; r < n; ++r) {
            Arrays.fill(cells[r], Cell.E);
        }
        turn = Cell.X;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        for (int r = 0; r < n; ++r) {
            //sb.append(r + 1);
            for (Cell cell: cells[r]) {
                if (cell == Cell.E) {
                    sb.append('.');
                } else {
                    sb.append(cell);
                }
            }
            sb.append('\n');
        }
        return sb.toString();
    }
    public int getM() {
        return m;
    }

    public int getN() {
        return n;
    }
    public Cell getTurn() {
        return turn;
    }

    @Override
    public Cell get(final int row, final int col) {
        return cells[row][col];
    }

    public boolean isValid(final Move move) {
        return 0 <= move.getRow() &&  move.getRow() < n
                && 0 <= move.getCol() &&  move.getCol() < m
                && get(move.getRow(), move.getCol()) == Cell.E
                && move.getValue() == turn;
    }

    public Result makeMove(final Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }
        empty--;
        int row = move.getRow();
        int col = move.getCol();
        cells[row][col] = move.getValue();
        for (int i = -1; i <= 0; i++) {
            for (int j = -1; j <= 1; j++) {
                if (i < 0 || j < 0) {
                    if (checkCells(i, j, move) + checkCells(-i, -j, move) - 1 >= k) {
                        return Result.WIN;
                    }
                }
            }
        }

        if (empty == 0) {
            return Result.DRAW;
        }

        turn = turn == Cell.X ? Cell.O : Cell.X;
        return Result.UNKNOWN;
    }
    private int checkCells(int iMove, int jMove, Move move) {
        int cnt = 0;
        int i = move.getRow();
        int j = move.getCol();

        while (i >= 0 && i < n && j >= 0 && j < m && cells[i][j] == move.getValue()) {
            i += iMove;
            j += jMove;
            cnt++;
        }
        return cnt;
    }
}