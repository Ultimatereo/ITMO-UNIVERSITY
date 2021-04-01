import java.util.Scanner;

public class ReverseMin {
	public static void main (String[] args) {
		int[] stringMin = new int [100001];
		int[] rowMin = new int [100001];
		int[] lines = new int [100001];
		int index = 0;
		int min, counter, currentInt;

		for (int i = 0; i < 100001; i++) {
			rowMin[i] = Integer.MAX_VALUE;
		}

		Scanner scanner = new Scanner(System.in);
		while (scanner.hasNextLine()) {
			Scanner lineScanner = new Scanner(scanner.nextLine());
			counter = 0;
			min = Integer.MAX_VALUE;

			while (lineScanner.hasNextInt()) {
				currentInt = lineScanner.nextInt();
				rowMin[counter] = Math.min(rowMin[counter], currentInt);
				counter += 1;
				min = Math.min(min, currentInt);
			}
			lineScanner.close();

			if (index == 0 && counter > 0) {
				index = 1;
			}

			for (int i = 0; i < counter; i++) {
				stringMin[index] = min;
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
				System.out.print(Math.min(stringMin[i], rowMin[curRow]) + " ");
				curRow += 1;
			}
		}
		System.out.println();
	}
}