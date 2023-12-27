package search;

//P: forall i = 0..|args| - 1: args[i] can be parsed into int &&
//   args это циклический сдвиг строго возрастающего массива
//Q: |args| == 0 && R = 0 || |args| > 0 && R = i, что args[i] = min(args)
public class BinarySearchShift {
    public static void main(String[] args) {
        // args.length == 0 && R == 0 || args.length > 0 && R = args[|args| - 1]
        int last = args.length > 0 ? Integer.parseInt(args[args.length - 1]) : 0;
        // last = R
        System.out.println(iterativeBinarySearchBruh(args, -1, args.length, last));
    }

    //P: forall i = 0..|args| - 1: args[i] can be parsed into int &&
    //   args это циклический сдвиг строго возрастающего массива && l >= -1 && r <= |arr| &&
    //   (|arr| == 0 && last == 0 || |arr| > 0 && last == arr[|arr| - 1])
    //Q: |arr| > 1 && R = i, что args[i] = min(args) || |arr| <= 1 && R = 0
    static int iterativeBinarySearchBruh(String[] arr, int l, int r, int last) {
        if (arr.length <= 1) {
            // |arr| <= 1
            return 0;
            // |arr| <= 1 && R = 0
        }
        // Invariant:
        // (l == 0 || (l > 0) && (arr[l] > last)) &&
        // (r == |arr| || (r < |arr|) && (arr[r] < last)) &&
        // r - l > 1 && |arr| > 1
        while (r - l > 1) {

            // Invariant && (2 * Middle == l + r || 2 * Middle + 1 == l + r)
            int m = l + (r - l) / 2;
            // Invariant && m == Middle

            // Invariant && m == Middle
            if (Integer.parseInt(arr[m]) < last) {
                // Invariant && m == Middle && arr[m] < last
                r = m;
                // Invariant && arr[m] < last && l == Middle
            } else {
                // Invariant && m == Middle && arr[m] > last
                l = m;
                // Invariant && arr[m] > last && r == Middle
            }
            // Invariant
        }
        // |arr| > 1 && (l == 0 || (l > 0) && (arr[l] > last)) &&
        // (r == |arr| || (r < |arr|) && (arr[r] < last)) &&
        // r - l <= 1 ( на самом деле даже r - l == 1 из ограничений на r - l,
        // на каждом шаге мы уменьшаем расстояние вдвое и минимальное возможное значение 1)

        if (r == arr.length) {
            // r == arr.length
            return r - 1;
            // R = r - 1 && r == arr.length
        }
        // r != arr.length
        return r;
        // R = r && r != arr.length
    }

    //P: forall i = 0..|args| - 1: args[i] can be parsed into int &&
    //   args это циклический сдвиг строго возрастающего массива && l >= -1 && r <= |arr| &&
    //   (|arr| == 0 && last == 0 || |arr| > 0 && last == arr[|arr| - 1])
    //Q: |arr| > 1 && R = i, что args[i] = min(args) || |arr| <= 1 && R = 0
    private static int recursiveBinarySearchBruh(String[] arr, int l, int r, int last) {
        if (arr.length <= 1) {
            // |arr| <= 1
            return 0;
            // |arr| <= 1 && R = 0
        }

        if (r - l <= 1) {
            if (r == arr.length) {
                //r - l <= 1 && Invariant && r == arr.length
                return r - 1;
            }
            //r - l <= 1 && Invariant && r != arr.length
            return r;
        }
        // Invariant && (2 * Middle == l + r || 2 * Middle + 1 == l + r)
        int m = l + (r - l) / 2;
        // Invariant && m == Middle
        if (Integer.parseInt(arr[m]) < last) {
            // Invariant && arr[m] < last
            return recursiveBinarySearchBruh(arr, l, m, last);
        } else {
            // Invariant && arr[m] > last
            return recursiveBinarySearchBruh(arr, m, r, last);
        }
    }
}
