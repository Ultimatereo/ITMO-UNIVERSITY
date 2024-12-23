# Parallel quick sort

<p align="start">
  <img src="pictures/img1.png" alt="Image 1" width="400"/>
  <img src="pictures/img2.png" alt="Image 2" width="400"/>
</p>



## Project structure
Everything is done in Kotlin. Here are descriptions of all the classes

| **Class Name**        | **Description**                                                                                      |
|------------------------|-----------------------------------------------------------------------------------------------------|
| **Sorter**            | A sealed class that serves as the base for sorting implementations, containing the `sort()` method. |
| **SequentialSorter**  | A data object that implements the `Sorter` interface and performs sorting sequentially (single-threaded). |
| **ParallelSorter**    | A data object that implements the `Sorter` interface and performs sorting using parallelism (multi-threaded). |
| **SorterTest**        | A unit test class that verifies the correctness of the sorting algorithms.                          |
| **SorterPerformanceTest** | A unit test class that measures and compares the performance of the sequential and parallel sorting algorithms. |

---

## Performance Testing Results

The sorting algorithms were tested on a **12th Gen Intel® Core™ i5-12450H processor** to evaluate their efficiency. The tests compared the performance of sequential and parallel sorting implementations using a randomly generated array of \(10^8\) elements, averaged over 5 iterations. Below are the detailed results:

### Sequential Sorting
The sequential sorting implementation processes the array on a single thread.

| **Iteration** | **Time (ms)** |
|---------------|---------------|
| 1             | 10003         |
| 2             | 9501          |
| 3             | 9590          |
| 4             | 9396          |
| 5             | 9731          |

**Average sequential sort time:** **9644.2 ms**

### Parallel Sorting
The parallel sorting implementation leverages multiple threads to process the array concurrently.

| **Iteration** | **Time (ms)** |
|---------------|---------------|
| 1             | 2818          |
| 2             | 2871          |
| 3             | 2881          |
| 4             | 2858          |
| 5             | 2923          |

**Average parallel sort time:** **2870.2 ms**

### Performance Comparison
The parallel sorting algorithm is approximately **3.36 times faster** than the sequential sorting algorithm.