import java.util.Scanner;

public class ReverseMax {
	public static void main (String[] args) {
		int[] stringMax = new int [100001];
		int[] rowMax = new int [100001];
		int[] lines = new int [100001];
		int index = 0;
		int max, counter, currentInt;

		for (int i = 0; i < 100001; i++) {
			rowMax[i] = Integer.MIN_VALUE;
		}

		Scanner scanner = new Scanner(System.in);
		while (scanner.hasNextLine()) {
			Scanner lineScanner = new Scanner(scanner.nextLine());
			counter = 0;
			max = Integer.MIN_VALUE;

			while (lineScanner.hasNextInt()) {
				currentInt = lineScanner.nextInt();
				rowMax[counter] = Math.max(rowMax[counter], currentInt);
				counter += 1;
				max = Math.max(max, currentInt);
			}
			lineScanner.close();

			if (index == 0 && counter > 0) {
				index = 1;
			}

			for (int i = 0; i < counter; i++) {
				stringMax[index] = max;
				index += 1;
			}

			if (scanner.hasNextLine()) {
				lines[index] += 1;
			}
		}
		scanner.close();


		int curRow = 0;
		for (int i = 0; i <= index; i++) {
			for (int j = 0; j < lines[i]; j++) {
				curRow = 0;
				System.out.println();
			}
			if (i != 0 && i != index) {
				System.out.print(Math.max(stringMax[i], rowMax[curRow]) + " ");
				curRow += 1;
			}
		}
		System.out.println();
	}
}