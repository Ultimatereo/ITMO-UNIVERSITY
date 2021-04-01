package game;

import java.util.Random;

public class RandomPlayer implements Player{
    private final Random random = new Random();
    private int n, m;
    @Override
    public Move makeMove(final Position position, final Cell cell) {
        this.n = position.getN();
        this.m = position.getM();
        Move move;
        do {
            move  = new Move(random.nextInt(n), random.nextInt(m), cell);
        } while (!position.isValid(move));
        return move;
    }
}