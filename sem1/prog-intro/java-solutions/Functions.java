public class Functions {
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

    public static void heapSort(int[] arr, int[] counters, int length) {
        for (int i = length - 1; i >= 0; i--) {
            heapify(arr, counters, length, i);
        }
        for (int i = length - 1; i > 0; i--) {
            swap(arr, 0, i);
            swap(counters, 0, i);
            heapify(arr, counters, i, 0);
        }
    }

    static void heapify(int[] arr, int[] counters, int n, int i) {
        int largest = i; // Initialize largest as root
        int l = 2 * i + 1; // left = 2*i + 1
        int r = 2 * i + 2; // right = 2*i + 2
        // If left child is larger than root
        if (l < n) {
            if (counters[l] > counters[largest]) {
                largest = l;
            } else if (counters[l] == counters[largest]) {
                if (arr[l] > arr[largest]) {
                    largest = l;
                }
            }
        }
        // If right child is larger than largest so far
        if (r < n) {
            if (counters[r] > counters[largest]) {
                largest = r;
            } else if (counters[r] == counters[largest]) {
                if (arr[r] > arr[largest]) {
                    largest = r;
                }
            }
        }
        // If largest is not root
        if (largest != i) {
            swap(arr, i, largest);
            swap(counters, i, largest);
            // Recursively heapify the affected sub-tree
            heapify(arr, counters, n, largest);
        }
    }

    public static void heapSort(int[] indexes, String[] arr, int length) {
        for (int i = length - 1; i >= 0; i--) {
            heapify(indexes, arr, length, i);
        }
        for (int i = length - 1; i > 0; i--) {
            swap(arr, i, 0);
            swap(indexes, i, 0);
            heapify(indexes, arr, i, 0);
        }
    }

    static void heapify(int[] indexes, String[] arr, int n, int i) {
        int largest = i;
        int l = 2 * i + 1;
        int r = 2 * i + 2;
        largest = getLargest(indexes, arr, n, largest, l);
        largest = getLargest(indexes, arr, n, largest, r);
        if (largest != i) {
            swap(arr, i, largest);
            swap(indexes, i, largest);
            heapify(indexes, arr, n, largest);
        }
    }

    private static int getLargest(int[] indexes, String[] arr, int n, int largest, int r) {
        if (r < n) {
            if (arr[r].compareTo(arr[largest]) > 0) {
                largest = r;
            } else if (arr[r].compareTo(arr[largest]) == 0) {
                if (indexes[r] > indexes[largest]) {
                    largest = r;
                }
            }
        }
        return largest;
    }

    static void swap(int[] arr, int i, int j) {
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }

    static void swap(String[] arr, int i, int j) {
        String temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }

    static int binarySearch(String[] arr, int l, int r, String x) {
        if (r >= l) {
            int mid = l + (r - l) / 2;
            if (arr[mid].equals(x))
                return mid;
            if (arr[mid].compareTo(x) > 0)
                return binarySearch(arr, l, mid - 1, x);
            return binarySearch(arr, mid + 1, r, x);
        }

        return -1;
    }

    /* This function takes last element as pivot, places
       the pivot element at its correct position in sorted
       array, and places all smaller (smaller than pivot)
       to left of pivot and all greater elements to right
       of pivot */
    static int partition(String[] arr, int[] counters, int low, int high) {

        // pivot
        int pivot = counters[high];

        // Index of smaller element and
        // indicates the right position
        // of pivot found so far
        int i = (low - 1);

        for(int j = low; j <= high - 1; j++) {
            // If current element is smaller
            // than the pivot
            if (counters[j] < pivot) {
                // Increment index of
                // smaller element
                i++;
                swap(arr, i, j);
                swap(counters, i, j);
            }
        }
        swap(arr, i + 1, high);
        swap(counters, i + 1, high);
        return (i + 1);
    }

    /* The main function that implements QuickSort
              arr[] --> Array to be sorted,
              low --> Starting index,
              high --> Ending index
     */
    static String[] quickSort(String[] arr, int[] counters, int low, int high) {
        if (low < high) {
            // pi is partitioning index, arr[p]
            // is now at right place
            int pi = partition(arr, counters, low, high);

            // Separately sort elements before
            // partition and after partition
            quickSort(arr, counters, low, pi - 1);
            quickSort(arr, counters, pi + 1, high);
        }
        return arr;
    }

    /* This function takes last element as pivot, places
       the pivot element at its correct position in sorted
       array, and places all smaller (smaller than pivot)
       to left of pivot and all greater elements to right
       of pivot */
    static int partition(String[] arr, int low, int high) {

        // pivot
        String pivot = arr[high];

        // Index of smaller element and
        // indicates the right position
        // of pivot found so far
        int i = (low - 1);

        for(int j = low; j <= high - 1; j++) {

            // If current element is smaller
            // than the pivot
            if (arr[j].compareTo(pivot) < 0) {

                // Increment index of
                // smaller element
                i++;
                swap(arr, i, j);
            }
        }
        swap(arr, i + 1, high);
        return (i + 1);
    }

    /* The main function that implements QuickSort
              arr[] --> Array to be sorted,
              low --> Starting index,
              high --> Ending index
     */
    static String[] quickSort(String[] arr, int low, int high) {
        if (low < high)
        {

            // pi is partitioning index, arr[p]
            // is now at right place
            int pi = partition(arr, low, high);

            // Separately sort elements before
            // partition and after partition
            quickSort(arr, low, pi - 1);
            quickSort(arr, pi + 1, high);
        }
        return arr;
    }
}
