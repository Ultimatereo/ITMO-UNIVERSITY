import java.util.*;
 
public class G {
    public static String WhatAreYouDoingStepBro(int n, int k) {
        String string = BinBruh(n, k);
        String answer = "";
        int count = 1;
        for (int i = 0; i < k - 1; i++) {
            if (string.substring(i, i + 1).equals("1")) {
                answer = answer + "~" + String.valueOf(count) + "&";
                count += 1;
            } else {
                answer = answer + String.valueOf(count) + "&";
                count += 1;
            }
        }
        if (string.substring(k - 1, k).equals("1")) {
            answer = answer + "~" + String.valueOf(k);
        } else {
            answer = answer + String.valueOf(k);
        }
        return answer;
    }
     
    public static long[] Bruh(long[] num, int n) {
        long[] a = new long[pow(2, n)];
        String string;
        for (int i = 0; i < pow(2, n); i++) {
            string = BinBruh(i, n);
            if (string.substring(0, 1).equals("1")) {
                a[i] = ~num[0];
            } else {
                a[i] = num[0];
            }
            for (int j = 1; j < n; j++) {
                if (string.substring(j, j + 1).equals("1")) {
                    a[i] = a[i] & ~num[j];
                } else {
                    a[i] = a[i] & num[j];
                }
            }   
        }
        return a;
    }
     
    public static String BinBruh(long x, int k) {
        String a = Long.toBinaryString(x);
        int n = a.length();
        for (int i = 0; i < (k - n); i++) {
            a = "0" + a;
        }
        return a;
    }
    public static int pow(int x, int y) { 
        int res = x;
        for (int i = 1; i < y; i++) {
            res = res * x;
        }
        return res;
    }
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        int n = input.nextInt();
        long[] num = new long[n];
        for (int i = 0; i < n; i++) {
            num[i] = input.nextLong();
        }
        long number = input.nextLong();
        input.close();
        String string = BinBruh(number, 64);
        if (number == 0) {
            System.out.println("1&~1");
        } else if (number == -1) {
            System.out.println("1|~1");
        } else {
            if (n == 1) {
                if (number == num[0]) {
                    System.out.println("1");
                } else if (number == (~num[0])) {
                    System.out.println("~1");
                } else {
                    System.out.println("Impossible");
                }
 
            }
            if (n > 1) {
                int[] needed = new int[pow(2, n)];
                long a[] = Bruh(num, n);
                for (int i = 0; i < 64; i++) {
                    if (string.substring(i, i + 1).equals("1")) {
                        for (int j = 0; j < pow(2, n); j++) {
                            if (BinBruh(a[j], 64).substring(i, i + 1).equals("1")) {
                                needed[j] = 1;
                                break;
                            }
                        }
                    }
                }
 
                System.err.println(Arrays.toString(a));
                System.err.println(Arrays.toString(needed));
                for (int i = 0; i < pow(2, n); i++) {
                    System.err.println(BinBruh(a[i], 64));
                }
                System.err.println("-----");
                System.err.println(BinBruh(number, 64));
                long check = 0;
                for (int i = 0; i < pow(2, n); i++) {
                    if (needed[i] == 1) {
                        check = check | a[i];
                    }
                }
                String answer = "";
                if (check == number) {
                    int last = 0;
                    for (int i = 0; i < pow(2, n); i++) {
                        if (needed[i] == 1) {
                            last = i;
                        }
                    }
                    System.err.println(last);
                    for (int i = 0; i < last; i++) {
                        if (needed[i] == 1) {
                            System.out.print(WhatAreYouDoingStepBro(i, n) + "|");
                        }
                    }
                    System.out.print(WhatAreYouDoingStepBro(last, n));
                } else {
                    System.out.println("Impossible");
                }
            }
        }
    }
}
