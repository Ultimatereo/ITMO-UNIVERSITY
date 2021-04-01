package game;

public interface Position {
    Cell get(int row, int col);
    int getN();
    int getM();
    boolean isValid(Move move);
}