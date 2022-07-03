package game;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Game {
    private final Board board;
    private final Player[] players;
    private final int numberOfPlayers;
    private List<Integer> banned = new ArrayList<>();

    public Game(Board board, int numberOfPlayers, Scanner in) {
        this.board = board;
        this.numberOfPlayers = numberOfPlayers;
        players = new Player[numberOfPlayers];
        for (int i = 1; i <= numberOfPlayers; i++) {
            System.out.println("Write the type of the " + i + " Player");
            System.out.println("There are 3 types of Players yet");
            System.out.println("1 - Random, 2 - Sequential, 3 - Human");
            while (true) {
                String line = in.nextLine();
                Scanner checker = new Scanner(line);
                if (check(checker)) continue;
                int type = checker.nextInt();
                if (!(1 <= type && type <= 3)) {
                    System.out.println("Please write an integer from 1 to 3 to choose the type of Player.");
                    continue;
                }
                switch (type) {
                    case 1 -> players[i - 1] = new RandomPlayer();
                    case 2 -> players[i - 1] = new SequentialPlayer();
                    case 3 -> players[i - 1] = new HumanPlayer(in);
                    default -> throw new AssertionError("Wrong type of Player was found!");
                }
                break;
            }
        }
    }
    private boolean check(Scanner checker) {
        if (!checker.hasNextInt()) {
            System.out.println("There is no integer in the line. Please write one integer number in the next line.");
            return true;
        }
        return false;
    }

    public int play(boolean log) {
        while (true) {
            for (int i = 0; i < numberOfPlayers; i++) {
                if (!banned.contains(i)) {
                    final int result = makeMove(players[i], i + 1, log);
                    if (result >= 0) {
                        return result;
                    }
                } else if (banned.size() == numberOfPlayers) {
                    return -1000; //Code for everyone being banned
                }
            }
        }
    }

    private int makeMove(Player player, int no, boolean log) {
        final Move move = player.makeMove(board.getPosition());
        final GameResult result = board.makeMove(move);
        if (log) {
            System.out.println();
            System.out.println("Player: " + no);
            System.out.println(move);
            System.out.println(board);
            System.out.println("Result: " + result);
        }
        switch (result) {
            case WIN:
                return no;
            case LOOSE:
                banned.add(no - 1);
                return -no;
            case DRAW :
                return 0;
            case UNKNOWN :
                return -100;
            default :
                throw new AssertionError("Unknown makeMove result " + result);
        }
    }
}
