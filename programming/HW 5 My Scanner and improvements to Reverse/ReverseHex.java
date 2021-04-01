import java.util.Arrays;
import java.io.*;

class ReverseChecker implements Checker {
    public boolean isWordCharacter(char c) {
        return !Character.isWhitespace(c);
    }
}

public class ReverseHex{
	private static int[] expandArraySize(int[] arr) {
        return Arrays.copyOf(arr, arr.length * 2);
    }
    private static String[] expandArraySizeString(String[] arr) {
        return Arrays.copyOf(arr, arr.length * 2);
    }
	private static ReverseChecker reverseChecker = new ReverseChecker();
	public static void main(String[] args) throws IOException{
		String[] result = new String[10];
		int[] lines = new int[10];
		int[] spacelines = new int[10];
		int count_string = 0; //Сколько чисел в строке
		int count = 0; //Сколько всего чисел
		int pp = 0; //Номер строки/Сколько строк
		Scanner input = new Scanner(System.in, reverseChecker);
		while (!input.isEmpty()) {
			count_string = 0;
			while (!input.isEndOfLine()){
				result[count] = input.next();
				count_string += 1;
				count += 1;
					
				if (count >= result.length) {
					result = expandArraySizeString(result);
				}
		
			}
			if (count_string == 0) {
				spacelines[pp] += 1;
				//System.out.println(Arrays.toString(spacelines));
			} else {
				lines[pp] = count_string;
				pp += 1;

				if (pp >= lines.length || pp >= spacelines.length) {
					lines = expandArraySize(lines);
					spacelines = expandArraySize(spacelines);
				}
				//System.out.println(Arrays.toString(lines));
				//System.out.println(Arrays.toString(result));
			}
			input.skipAllLine();
		}
		input.close();
		//System.out.println(Arrays.toString(lines));
		//System.out.println(Arrays.toString(result));		
		int k = pp - 1;
		int i = count - 1;
		if (i < 0) {
			for (int t = 0; t < spacelines[0]; t++) {
				System.out.println();
			}
		} else {
			for (int j = 0; j < spacelines[pp]; j++) {
				System.out.println();
			}
			while (i >= 0) {
				for (int j = 0; j < lines[k]; j++){
					System.out.print(result[i] + " ");
					i -= 1;
				}
				for (int j = 0; j < spacelines[k]; j++) {
					System.out.println();
				}
				k -= 1;
				System.out.println();
			}
		}
	}
}