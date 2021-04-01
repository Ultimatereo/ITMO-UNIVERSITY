package game;
import java.util.HashMap;
import java.util.Random;

public class Match {
    private int score1, score2;
    public Match(Table table, int winScore, int n, int m, int k, Player player1, Player player2, int playerNo1, int playerNo2) {
        this.score1 = 0;
        this.score2 = 0;
        HashMap<Integer, Integer> numbers = new HashMap<>();
        numbers.put(0, playerNo1 - 1);
        numbers.put(1, playerNo2 - 1);
        HashMap<Integer, Player> players = new HashMap<>();
        players.put(playerNo1 - 1, player1);
        players.put(playerNo2 - 1, player2);
        int a = 1;
        while (score1 != winScore && score2 != winScore) {
            final Random random = new Random();
            a = random.nextInt(2);
            System.out.println("Player " + (numbers.get(a) + 1) + " plays with Player " + (numbers.get(1 - a) + 1));
            final Game game = new Game(players.get(numbers.get(a)), players.get(numbers.get(1 - a)), true);
            int res = game.play(new Board(n, m, k));
            if (res == 1) {
                System.out.println("X's won in current round");
                System.out.println("Game result: Player " + (numbers.get(a) + 1));
                score1++;
            } else if (res == 2) {
                System.out.println("O's won in current round");
                System.out.println("Game result: Player" + (numbers.get(1 - a) + 1));
                score2++;
            } else {
                System.out.println("Draw");
            }
        }
        if (score1 == winScore) {
            System.out.println("Player " + (numbers.get(a) + 1) + " won in this match");
            table.add(numbers.get(a), 3);
        } else {
            System.out.println("Player " + (numbers.get(1 - a) + 1) + " won in this match");
            table.add(numbers.get(1 - a), 3);
        }
    }
}
