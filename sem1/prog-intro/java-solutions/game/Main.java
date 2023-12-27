package game;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        final int result = new Game(
                new TicTacToeBoard(
                        Integer.parseInt(args[0]),
                        Integer.parseInt(args[1]),
                        Integer.parseInt(args[2]),
                        Integer.parseInt(args[3])),
                        Integer.parseInt(args[3]), new Scanner(System.in)
        ).play(true);
        if (result == -1000) {
            System.out.println("Every player was banned from the game.");
        } else if (result > 0){
            System.out.println("Player " + result + " won!");
        } else {
            System.out.println("DRAW");
        }
    }
}
