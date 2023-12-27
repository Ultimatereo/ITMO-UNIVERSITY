import java.util.*;
 
public class B {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        int n = input.nextInt();
        int[] facts = new int[5];
        for (int i = 0; i < 5; i++) {
            facts[i] = 1;
        }
        for (int bruh = 0; bruh < n; bruh++) {
            int count = input.nextInt();
            String num = input.next(); 
            int[] string = new int[num.length()];
            for (int i = 0; i < num.length(); i++) {
                string[i] = Integer.parseInt(num.substring(i, i + 1));
            }
            int k = (int)Math.pow(2, count);
            if ((count == 0) && (string[0] == 1)) {
                facts[0] = 0;
                facts[2] = 0;
            }
            if ((count == 0) && (string[0] == 0)) {
                facts[1] = 0;
                facts[2] = 0;
            }
            //System.out.println(count + " " + num);
            //System.out.println(num.substring(0, 1));
            //System.out.println(num.substring(1, 2));
            if (facts[0] == 1) {
                if (string[0] == 1) {
                    facts[0] = 0;
                }
            }
            if (facts[1] == 1) {
                if (string[string.length - 1] == 0) {
                    facts[1] = 0;
                }
            }
            if (facts[2] == 1) {
                for (int i = 0; i < k/2; i++) {
                    if (string[i] == string[string.length - i - 1]) {
                        facts[2] = 0;
                        break;
                    }
                }
            }
            if (facts[3] == 1) {
                for (int i = 0; i < k; i++) {
                    if (facts[3] == 0) {
                        break;
                    }
                    for (int j = i + 1; j < k; j++) {
                        if (((i & j) == i) && (string[i] == 1) && (string[j] == 0)) {
                            facts[3] = 0;
                            break;
                        }
                    }
                }
            }
            if (facts[4] == 1) {
                int[] lin = new int[k];
                int tt = k;
                lin[0] = string[0];
                for (int i = 1; i < k; i++) {
                    lin[i] = 0;
                }
                for (int t = 1; t < k; t++) {
                    for (int i = 0; i < tt - 1; i++) {
                        string[i] = (string[i] + string[i + 1]) % 2;
                    }
                    lin[t] = string[0];
                    tt -= 1;
                }
                for (int i = 1; i < k; i++) {
                    if (((i & (i - 1)) > 0) && (lin[i] == 1)) {
                        facts[4] = 0;
                        break;
                    }
                }
            }
        }
        input.close();
        int c = 0;
        for (int i = 0; i < 5; i++) {
            if (facts[i] == 1) {
                c += 1;
            }
        }
        if (c > 0) {
            System.out.println("NO");
        } else {
            System.out.println("YES");
        }
 
    }
}
