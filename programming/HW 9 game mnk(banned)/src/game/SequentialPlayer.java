package game;

public class SequentialPlayer implements Player {
    private int n, m;
    @Override
    public Move makeMove(final Position position, final Cell cell) {
        this.n = position.getN();
        this.m = position.getM();
        for (int row = 0; row < n; ++row) {
            for (int col = 0; col < m; ++col) {
                if (position.get(row, col) == Cell.E) {
                    final Move move = new Move(row, col, cell);
                    if (position.isValid(move)) {
                        return move;
                    }
                }
            }
        }
        throw new AssertionError("No empty cells");
    }
}