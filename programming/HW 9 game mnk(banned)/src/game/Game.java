package game;

public class Game {
    private final Player player1;
    private final Player player2;
    private final boolean enableLogging;

    public Game(final Player player1, final Player player2, boolean enableLogging) {
        this.player1 = player1;
        this.player2 = player2;
        this.enableLogging = enableLogging;
    }

    public int play(final Board board) {
        int result;
        while (true) {
            result = makeMove(board, player1, 1);
            if (result != -1) {
                return result;
            }
            result = makeMove(board, player2, 2);
            if (result != -1) {
                return result;
            }
        }
    }

    private int makeMove(final Board board, final Player player, final int playerNo){
        if (enableLogging) {
            System.out.println(board);
        }
        final Move move = player.makeMove(board, board.getTurn());

        if(enableLogging) {
            System.out.println("Move: " + move);
        }

        final Result result = board.makeMove(move);
        if (result == Result.UNKNOWN) {
            return -1;
        }
        if (enableLogging) {
            System.out.println(board);
        }
        if (result == Result.WIN) {
            return playerNo;
        } else if (result == Result.LOSE) {
            return 3 - playerNo;
        } else if (result == Result.DRAW) {
            return 0;
        } else {
            throw new AssertionError("Unknown result type " + result);
        }
    }
}