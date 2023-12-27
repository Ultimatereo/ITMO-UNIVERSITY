import java.util.Arrays;
import java.util.Scanner;

public class ReverseTranspose {

    public static int[][] add(int[][] arr, int[] x, int i) {
        if (i == arr.length) {
            int[][] newArr = new int[arr.length * 2][];
            System.arraycopy(arr, 0, newArr, 0, arr.length);
            newArr[arr.length] = x;
            return newArr;
        }

        arr[i] = x;
        return arr;
    }

    public static int[] add(int[] arr, int x, int i) {
        if (i == arr.length) {
            int[] newArr = new int[arr.length * 2];
            System.arraycopy(arr, 0, newArr, 0, arr.length);
            newArr[arr.length] = x;
            return newArr;
        }

        arr[i] = x;
        return arr;
    }

    public static void main(String[] args) {
        int[] mas;
        int[][] result = new int[64][];
        int[] sizeOfStrings = new int[64];
        int countGoodString = 0;
        int countInt;
        int maxStringSize = 0;
        Scanner scanner = new Scanner(System.in);

        while (scanner.hasNextLine()) {
            String string = scanner.nextLine();
            Scanner stringScanner = new Scanner(string);
            countInt = 0;
            mas = new int[64];
            while (stringScanner.hasNextInt()) {
                mas = add(mas, stringScanner.nextInt(), countInt);
                countInt++;
            }
            stringScanner.close();

            if (countInt > 0) {
                maxStringSize = Math.max(countInt, maxStringSize);
                sizeOfStrings = add(sizeOfStrings, countInt, countGoodString);
                result = add(result, mas, countGoodString++);
            }
        }
        scanner.close();

//        for (int i = 0; i < countGoodString; i++) {
//            System.err.println("mas: " + i + " " + Arrays.toString(result[i]));
//        }

//        System.err.println("sizeOfStrings: " + Arrays.toString(sizeOfStrings));
//        System.err.println("countString: " + countString);
//        System.err.println("countGoodString: " + countGoodString);
//        System.err.println("maxStringSize: " + maxStringSize);


        for (int j = 0; j < maxStringSize; j++) {
            for (int i = 0; i < countGoodString; i++) {
                if (j < sizeOfStrings[i]) {
                    System.out.print(result[i][j] + " ");
                }
            }
            System.out.println();
        }

    }
}