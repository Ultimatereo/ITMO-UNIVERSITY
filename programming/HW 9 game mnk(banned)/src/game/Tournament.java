package game;

import java.util.Random;

public class Tournament {
    private Player[] queue;

    public Tournament(int tourNo, int playerNo, int winScore, int n, int m, int k, Player[] queue) {
        this.queue = queue;
        Table table = new Table(playerNo);
        for (int tour = 0; tour < tourNo; tour++) {
            for (int playerNo1 = 1; playerNo1 <= playerNo; playerNo1++) {
                for (int playerNo2 = playerNo1 + 1; playerNo2 <= playerNo; playerNo2++) {
                    final Match match = new Match(table, winScore, n, m, k, queue[playerNo1 - 1], queue[playerNo2 - 1], playerNo1, playerNo2);
                }
            }
            System.out.println(table.toString());
        }
    }
}