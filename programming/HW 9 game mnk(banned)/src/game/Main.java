package game;

import java.util.NoSuchElementException;
import java.util.Scanner;

public class Main {
    public static void main (String[] args) {
        Scanner input = new Scanner(System.in);
        Scanner lineReader;
        int m, n, k;
        do {
            System.out.println("Give the values of n, m, k");
            lineReader = new Scanner(input.nextLine());
            try {
                n = lineReader.nextInt();
                m = lineReader.nextInt();
                k = lineReader.nextInt();
            }
            catch (NoSuchElementException e) {
                n = -1;
                m = -1;
                k = -1;
            }
        } while (n < 1 || m < 1 || k < 1);


        int tourNo, playerNo, winScore;
        do{
            System.out.println("Give the number of tours, the number of players and the winner score of every match: ");
            lineReader = new Scanner(input.nextLine());
            try {
                tourNo = lineReader.nextInt();
                playerNo = lineReader.nextInt();
                winScore = lineReader.nextInt();
            } catch (NoSuchElementException e) {
                tourNo = 0;
                playerNo = 0;
                winScore = -1;
            }
        } while (tourNo < 1 || playerNo < 2 || winScore < 1);

        Player [] queue = new Player[playerNo];
        for (int i = 0; i < playerNo; i++) {
            int num;
            do {
                System.out.println("Give the Type of Players: RandomPlayer - 1, SequentialPlayer - 2, HumanPlayer - 3 ");
                lineReader = new Scanner(input.nextLine());
                try {
                    num = lineReader.nextInt();
                } catch (NoSuchElementException e) {
                    num = 0;
                }
            } while (num < 1 || num > 3);

            if (num == 1) {
                queue[i] = new RandomPlayer();
            } else if (num == 2) {
                queue[i] = new SequentialPlayer();
            } else {
                queue[i] = new HumanPlayer();
            }
        }
        new Tournament(tourNo, playerNo, winScore, n, m, k, queue);
    }
}