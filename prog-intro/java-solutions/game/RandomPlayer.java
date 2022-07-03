package game;

import java.util.Random;

public class RandomPlayer implements Player {
    private final Random random = new Random();

    @Override
    public Move makeMove(Position position) {
        int row = position.getRowSize();
        int col = position.getColSize();
        while (true) {
            final Move move = new Move(
                    random.nextInt(row),
                    random.nextInt(col),
                    position.getTurn()
            );
            if (position.isValid(move)) {
                return move;
            }
        }
    }
}
