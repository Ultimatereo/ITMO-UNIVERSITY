public class ReverseHex2 {
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

    public static String[] add(String[] arr, String x, int i) {
        if (i == arr.length) {
            String[] newArr = new String[arr.length * 2];
            System.arraycopy(arr, 0, newArr, 0, arr.length);
            newArr[arr.length] = x;
            return newArr;
        }

        arr[i] = x;
        return arr;
    }

    public static void main(String[] args) {
        String[] mas = new String[64];
        int[] sizeOfStrings = new int[64];
        int countString = 0;
        int countAll = 0;
        int countInt;
//        char[] lineSeparator = System.lineSeparator().toCharArray();
//        System.err.println(lineSeparator.length);
//        for (int i = 0; i < lineSeparator.length; i++) {
//            System.err.println(i + " " + (int) lineSeparator[i]);
//        }
        try {
            Scanner scanner = new Scanner(System.in);
            while (scanner.hasInput()) {
                countInt = 0;
                while (scanner.hasCharInLine()) {
                    String integer = scanner.nextHex();
                    //System.err.println("integer: " + integer);
                    mas = add(mas, integer, countAll);
//                    System.err.println("new array: " + Arrays.toString(mas));
                    countInt++;
                    countAll++;
                    //System.err.println(countInt + " " + countAll);
                }
                //System.err.println("countInt: " + countInt);
                if (countInt > 0) {
                    sizeOfStrings = add(sizeOfStrings, countInt, countString);
                } else {
                    sizeOfStrings = add(sizeOfStrings, 0, countString);
                }
                countString++;
                //scanner.skipAllLine();
            }
        } catch (Exception e) {
            System.err.println("Error occurred!");
            e.printStackTrace();
        }
//        System.err.println("mas: " + Arrays.toString(mas));
//        System.err.println("sizeOfStrings: " + Arrays.toString(sizeOfStrings));
//        System.err.println("countString: " + countString);
//        System.err.println("countAll: " + countAll);
        int k = countAll - 1;

        for (int i = countString - 1; i >= 0; i--) {
            for (int j = 0; j < sizeOfStrings[i]; j++) {
                System.out.print(mas[k--] + " ");
            }
            System.out.println();
        }

    }
}
