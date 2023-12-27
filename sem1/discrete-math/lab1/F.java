import java.util.*;
 
public class F {
    public static int[] OneForAll(int n, int k, int[][] x) {
        int[] one = new int[k];
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < n; j++) {
                if (x[i][j] != -1) {
                    one[i] += 1;
                }
            }
        }
 
        int[] bruh = new int[n + 1];
        for (int i = 0; i < n; i++) {
            bruh[i] = -1;
        }
        int key = 0;
        for (int i = 0; i < k; i++) {
            if (one[i] == 1) {
                for (int j = 0; j < n; j++) {
                    if (x[i][j] != -1) {
                        if ((bruh[j] != -1) && (bruh[j] != x[i][j])) {
                            key = 1;
                        } else {
                            bruh[j] = x[i][j];
                        }
                    }
                }
            }
        }
        int count = 0;
        for (int i = 0; i < k; i++) {
            if (one[i] > 1) {
                count = 0;
                for (int j = 0; j < n; j++) {
                    if (((x[i][j] == 0) && (bruh[j] == 1)) || ((x[i][j] == 1) && (bruh[j] == 0)) || (x[i][j] == -1)) {
                        count += 1;
                    }
                }
                //System.out.println("count: " + count);
                if (count == n) {
                    key = 1;
                    break;
                }
            }
        }
        bruh[n] = key;
        return bruh;
 
    }
    public static int[][] putZero(int n, int k, int[][] x, int pp) { // x_k элемент приравнивается нулю
        for (int i = 0; i < k; i++) {
            if (x[i][pp] == 0) {
                for (int j = 0; j < n; j++) {
                    x[i][j] = -1;
                }
            } 
            if (x[i][pp] == 1) {
                x[i][pp] = -1;
            }
        }
        return x;
    } 
    public static int[][] putOne(int n, int k, int[][] x, int pp) { // x_k элемент приравнивается единице
        for (int i = 0; i < k; i++) {
            if (x[i][pp] == 1) {
                for (int j = 0; j < n; j++) {
                    x[i][j] = -1;
                }
            } 
            if (x[i][pp] == 0) {
                x[i][pp] = -1;
            }
        }
        return x;
    } 
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        int n = input.nextInt(); // Количество столбцов
        int k = input.nextInt(); // Количество строк
        int [][] x = new int[100][100];
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < n; j++) {
                x[i][j] = input.nextInt();
            }
        }
        int count;
        int[] mas;
        input.close();
        while (true) {
            mas = OneForAll(n, k, x);
            System.err.println("OneForAll: " + Arrays.toString(mas));
            if (mas[n] == 1) {
                System.out.println("YES");
                break;
            }
            count = 0;
            for (int i = 0; i < n; i++) {
                if (mas[i] != -1) {
                    count += 1;
                }
            }
            if (count == 0) {
                System.out.println("NO");
                break;
            }
 
            for (int i = 0; i < n; i++) {
                if (mas[i] == 0) {
                    x = putZero(n, k, x, i);
                }
                if (mas[i] == 1) {
                    x = putOne(n, k, x, i);
                }
            }
             
            System.err.println("------");
            for (int i = 0; i < k; i ++) {
                for (int j = 0; j < n; j++) {
                    System.err.print(x[i][j] + " ");
                }
                System.err.println();
            }
             
        }
        //System.out.println(Arrays.toString(OneForAll(n, k, x)));
    }
}
