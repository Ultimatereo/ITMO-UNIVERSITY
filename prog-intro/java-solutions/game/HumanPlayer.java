package game;

import java.util.Scanner;

public class HumanPlayer implements Player {
    private final Scanner in;

    public HumanPlayer(Scanner in) {
        this.in = in;
    }

    @Override
    public Move makeMove(Position position) {
        System.out.println();
        System.out.println("Current position");
        System.out.println(position);
        System.out.println("Enter your move for " + position.getTurn());
        while (true) {
            String line = in.nextLine();
            Scanner checker = new Scanner(line);
            if (check(checker)) continue;
            int row = checker.nextInt() - 1;
            if (check(checker)) continue;
            int col = checker.nextInt() - 1;
            Move move = new Move(row, col, position.getTurn());
            if (!position.isValid(move)) {
                System.out.println("Your inputs are invalid. Please enter them again.");
                continue;
            }
            return move;
        }
    }

    private boolean check(Scanner checker) {
        if (!checker.hasNextInt()) {
            System.out.println("You need to write two integer numbers, dummy. Do you know what numbers are?");
            return true;
        }
        return false;
    }
}
