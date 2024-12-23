# Parallel BFS

<p align="start">
  <img src="pictures/img1.png" alt="Image 1" width="400"/>
  <img src="pictures/img2.png" alt="Image 2" width="400"/>
</p>



## Project structure
Everything is done in Kotlin. Here are descriptions of all the classes

| **Class Name**            | **Description**                                                                                                                                 |
|---------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------|
| **BFSImplementation**     | A sealed class that serves as the base for bfs implementations, containing the `bfs()` method.                                                  |
| **SequentialBFS**         | A data object that implements the `BFSImplementation` interface and performs bfs sequentially (single-threaded) using queue.                    |
| **ParallelBFS**           | A data object that implements the `BFSImplementation` interface and performs bfs using parallelism (multi-threaded) and frontier approach.      |
| **BFSImplementationTest** | A unit test class that verifies the correctness of the bfs algorithms and checks that both sequential and parallel approach return same values. |
| **BFSPerformanceTest**    | A unit test class that measures and compares the performance of the sequential and parallel bfs algorithms.                                     |

---

## Performance Testing Results

The BFS algorithms were tested on a **12th Gen Intel® Core™ i5-12450H processor** to evaluate their efficiency. The tests compared the performance of sequential and parallel BFS implementations using a cubic graph with the side of 300 nodes, averaged over 5 iterations. Below are the detailed results:

### Sequential BFS

| **Iteration** | **Time (ms)** |
|---------------|---------------|
| 1             | 10822         |
| 2             | 11267         |
| 3             | 17729         |
| 4             | 23601         |
| 5             | 14391         |

**Average sequential BFS time:** **15562.0 ms**

### Parallel BFS

| **Iteration** | **Time (ms)** |
|---------------|---------------|
| 1             | 5415         |
| 2             | 4253         |
| 3             | 4219         |
| 4             | 4154         |
| 5             | 4267         |

**Average parallel BFS time:** **4461.6 ms**

### Performance Comparison
The parallel BFS algorithm is approximately **3.49 times faster** than the sequential BFS algorithm.

### Additional Comments
Results are not really re