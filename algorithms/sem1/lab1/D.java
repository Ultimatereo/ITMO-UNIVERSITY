import java.util.Scanner;
 
public class D {
	public static void main (String[] args) {
		int[] a = new int[100000];
		int[] output = new int[50001];
		int size = 0;
		int count = 0;
		int index, c, t;
		Scanner input = new Scanner(System.in);
		int n = input.nextInt();
		for (int i = 0; i < n; i++) {
			index = input.nextInt();
			if (index == 0) {
				c = input.nextInt();
				a[size] = c;
				size += 1;
				int j = size - 1;
				while ((j > 0) && (a[j - 1] < a[j])) {
					t = a[j];
					a[j] = a[j - 1];
					a[j - 1] = t;
					j -= 1;
				}
			}
			if (index == 1) {
				output[count] = a[count];
				a[count] = Integer.MAX_VALUE;
				count += 1;
			}
		}
		input.close();
		for (int i = 0; i < count; i++) {
			System.out.println(output[i]);
		}
	}
}
