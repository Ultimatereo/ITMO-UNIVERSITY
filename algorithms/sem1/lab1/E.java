import java.util.*;
 
public class E {
	public static int[] merge (int[] a, int[] b) {
		int n = a.length;
		int m = b.length;
		int[] c = new int[n + m];
		int i = 0;
		int j = 0;
		int k = 0;
		while ((i < n) || (j < m)) {
			if ((j == m) || (i < n && a[i] < b[j])) {
				c[k++] = a[i++];
			} else {
				c[k++] = b[j++];
			}
		}
		return c;
	}
	public static int[] sort(int[] a) {
		int n = a.length;
		if (n < 2) {
			return a;
		}
		int[] al = new int[a.length / 2];
		System.arraycopy(a, 0, al, 0, a.length / 2);
		int[] ar = new int[a.length - a.length/2];
		System.arraycopy(a, a.length / 2, ar, 0, a.length - a.length / 2);
		al = sort(al);
		ar = sort(ar);
		return merge(al, ar);
	}
	public static int firstIndex(int[] a, int x, int n) {
		int l = 0;
		int r = n + 1;
		int m;
		while (r - l > 1) {
			m = (l + r)/2;
			if (a[m] < x) {
				l = m;
			} else {
				r = m;
			}
		}
		return r; 
	}
	public static int secondIndex(int[] a, int x, int n) {
		int l = 0;
		int r = n + 1;
		int m;
		while (r - l > 1) {
			m = (l + r)/2;
			if (a[m] <= x) {
				l = m;
			} else {
				r = m;
			}
		}
		return l; 
	}
	public static void main (String[] args) {
		int l, r, leftIndex, rightIndex;
		Scanner input = new Scanner(System.in);
		int n = input.nextInt();
		int[] array = new int[n + 2];
		array[0] = Integer.MIN_VALUE;
		array[n + 1] = Integer.MAX_VALUE;
		for (int i = 1; i < n + 1; i++) {
			array[i] = input.nextInt();
		}
		array = sort(array);
		//System.out.println(Arrays.toString(array));
		int k = input.nextInt();
		int[] output = new int[k]; 
		for (int i = 0; i < k; i++) {
			l = input.nextInt();
			r = input.nextInt();
			leftIndex = firstIndex(array, l, n);
			rightIndex = secondIndex(array, r, n);
 
 
			//System.out.println(leftIndex);
			//System.out.println(rightIndex);
 
			output[i] = rightIndex - leftIndex + 1;
			
		}
		input.close();
		for (int i = 0; i < k-1; i++) {
			System.out.print(output[i] + " ");
		}
		System.out.print(output[k-1]);
	}
}
