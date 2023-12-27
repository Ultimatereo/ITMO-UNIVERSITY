package search;

//P: args.length > 0 && forall i = 0..|args| - 1: args[i] can be parsed into int &&
//   args нестрого убывает на i = 1..|args| - 1
//Q: R = min(i) - 1 из i от 1 до args.length - 1, при которых args[i] <= args[0] или |args| - 1, если нет подходящего числа
public class BinarySearch {
    public static void main(String[] args) {
        //P: args.length > 0 && forall i = 0..|args| - 1: args[i] can be parsed into int &&
        //   args нестрого убывает на i = 1..|args| - 1 && args[0] = X
        int n = Integer.parseInt(args[0]);
        //Q: args.length > 0 && forall i = 0..|args| - 1: args[i] can be parsed into int &&
        //   args нестрого убывает на i = 1..|args| - 1 && n = X
        System.out.println(recursiveBinarySearch(args, n, 0, args.length));
    }

    //P: |arr| > 0 && forall i = 0..|arr| - 1: arr[i] can be parsed into int &&
    //   arr нестрого убывает на i = 1..|arr| - 1 && l >= 0 && r <= |arr|
    //Q: R = минимальное значение индекса i, при котором arr[i] <= x или |arr|, если нет подходящего числа
    static int iterativeBinarySearch(String[] arr, int x, int l, int r) {
        // Invariant: |arr| > 0 &&
        // (l == 0 || (l > 0) && (arr[l] > x)) &&
        // (r == |arr| || (r < |arr|) && (arr[r] <= x)) &&
        // r - l > 1
        while (r - l > 1) {

            // Invariant && (2 * Middle == l + r || 2 * Middle + 1 == l + r)
            int m = l + (r - l) / 2;
            // Invariant && m == Middle

            // Invariant && m == Middle
            if (Integer.parseInt(arr[m]) > x) {
                // Invariant && m == Middle && arr[m] > x
                l = m;
                // Invariant && arr[m] > x && l == Middle
            } else {
                // Invariant && m == Middle && arr[m] <= x
                r = m;
                // Invariant && arr[m] <= x && r == Middle
            }
            // Invariant
        }
        // |arr| > 0 &&
        // (l == 0 || (l > 0) && (arr[l] > x)) &&
        // (r == |arr| || (r < |arr|) && (arr[r] <= x)) &&
        // r - l <= 1 ( на самом деле даже r - l == 1 из ограничений на r - l,
        // на каждом шаге мы уменьшаем расстояние вдвое и минимальное возможное значение 1)

        // Отсюда, R - искомое число или правая граница
        return r - 1;
    }

    //P: |arr| > 0 && forall i = 0..|arr| - 1: arr[i] can be parsed into int &&
    //   arr нестрого убывает на i = 1..|arr| - 1 && l >= 0 && r <= |arr| =: Pre
    //Q: R = минимальное значение индекса i, при котором arr[i] <= x или |arr|, если нет подходящего числа
    static int recursiveBinarySearch(String[] arr, int x, int l, int r) {
        if (r - l <= 1) {
            //r - l <= 1 && Invariant
            return r - 1;
        }
        //(2 * Middle == l + r || 2 * Middle + 1 == l + r)
        int m = l + (r - l) / 2;
        // m == Middle

        if (Integer.parseInt(arr[m]) > x) {
            //Invariant && arr[m] > x
            return recursiveBinarySearch(arr, x, m, r);
        } else {
            //Invariant && arr[m] <= x
            return recursiveBinarySearch(arr, x, l, m);
        }
    }
}
