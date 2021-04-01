import java.util.Scanner;
 
public class H {
    public static String plus(String x, String y) {
        String answer = "((" + x + "|" + x + ")|(" + y + "|" + y + "))";
        return answer; 
    }
    public static String multi(String x, String y) {
        String answer = "((" + x + "|" + y + ")|(" + x + "|" + y + "))";
        return answer;
    }
    public static String median(String x, String y, String z) {
        String answer = "((" + x + "|" + plus(y, z) + ")|(" + y + "|" + z + "))";
        return answer;
    }
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        int n = input.nextInt();
        input.close();
        String a = "A0";
        String b = "B0";
        String c = multi(a, b);
        for (int i = 1; i < n; i++) {
            a = "A" + String.valueOf(i);
            b = "B" + String.valueOf(i);
            c = median(c, a, b);
        }
        System.out.println(c);
    }
}
