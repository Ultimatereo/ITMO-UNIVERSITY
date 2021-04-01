import java.util.*;
 
public class D {
    public static int pow(int x, int y) { 
        int res = x;
        for (int i = 1; i < y; i++) {
            res = res * x;
        }
        return res;
    }
    public static void main(String[] args) {
        String[] num = new String[1024];
        int[] res = new int[1024];
        int x = 0;
        Scanner input = new Scanner(System.in);
        int n = input.nextInt();
        for (int i = 0; i < pow(2, n); i++) {
            num[i] = input.next();
            res[i] = input.nextInt();
            if (res[i] == 1) {
                x += 1;
            }
        }
        input.close();
        int key;
        if (x <= pow(2, n-1)) {
            key = 1;
        } else {
            key = 0;
            x = pow(2, n) - x;
        }
        //System.out.println(key + " " + x);
        //System.out.println(2*n + (n-1)*x + (x-1));
        int count = 2*n;
        if (x == 0) {
            System.out.println(n + 2);
            if (res[0] == 0) {
                System.out.println("1 1");
                System.out.println("2 1 " + (n + 1));
            } else {
                System.out.println("1 1");
                System.out.println("3 1 " + (n + 1));           
            }
        } else if (key == 1) {
            System.out.println(2*n + (n-1)*x + (x-1));
            for (int i = 1; i <= n; i++) {
                System.out.println("1 " + String.valueOf(i));
            }
            for (int i = 0; i < pow(2, n); i++) {
                if (res[i] == key) {
                    //System.out.println(num[i].substring(0, 1) + " " + num[i].substring(1, 2));
                    //System.out.println(num[i].substring(0, 1).equals("1"));
                    //System.out.println(num[i].substring(1, 2) == "1");
                    System.out.print("2 ");
                    if (num[i].substring(0, 1).equals("1")) {
                        System.out.print("1 ");
                    } else {
                        System.out.print(1 + n);
                        System.out.print(" ");
                    }
                    if (num[i].substring(1, 2).equals("1")) {
                        System.out.println("2");
                    } else {
                        System.out.println(2 + n);
                    }
                    count += 1;
                    for (int k = 2; k < n; k++) {
                        if (num[i].substring(k, k + 1).equals("1")) {
                            System.out.println("2 " + (k + 1) + " " + count);
                        } else {
                            System.out.println("2 " + (k + n + 1) + " " + count);
                        }
                        count += 1;
                    }
                }
            }
            if (x > 1) {
                int pp = 3*n - 1;
                System.out.println("3 " + pp + " " + (pp + n - 1));
                count += 1;
                pp = pp + 2*n - 2;
                for (int i = 0; i < x - 2; i++) {
                    System.out.println("3 " + pp + " " + count);
                    pp = pp + n - 1;
                    count += 1;
                }
            }
        } else {
            System.out.println(2*n + (n-1)*x + (x-1));
            for (int i = 1; i <= n; i++) {
                System.out.println("1 " + String.valueOf(i));
            }
            for (int i = 0; i < pow(2, n); i++) {
                if (res[i] == key) {
                    //System.out.println(num[i].substring(0, 1) + " " + num[i].substring(1, 2));
                    //System.out.println(num[i].substring(0, 1).equals("1"));
                    //System.out.println(num[i].substring(1, 2) == "1");
                    System.out.print("3 ");
                    if (num[i].substring(0, 1).equals("0")) {
                        System.out.print("1 ");
                    } else {
                        System.out.print(1 + n);
                        System.out.print(" ");
                    }
                    if (num[i].substring(1, 2).equals("0")) {
                        System.out.println("2");
                    } else {
                        System.out.println(2 + n);
                    }
                    count += 1;
                    for (int k = 2; k < n; k++) {
                        if (num[i].substring(k, k + 1).equals("0")) {
                            System.out.println("3 " + (k + 1) + " " + count);
                        } else {
                            System.out.println("3 " + (k + n + 1) + " " + count);
                        }
                        count += 1;
                    }
                }
            }
            if (x > 1) {
                int pp = 3*n - 1;
                System.out.println("2 " + pp + " " + (pp + n - 1));
                count += 1;
                pp = pp + 2*n - 2;
                for (int i = 0; i < x - 2; i++) {
                    System.out.println("2 " + pp + " " + count);
                    pp = pp + n - 1;
                    count += 1;
                }
            }
        }
 
    }
}
