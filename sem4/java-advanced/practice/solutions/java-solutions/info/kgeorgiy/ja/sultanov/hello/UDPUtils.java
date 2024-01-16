package info.kgeorgiy.ja.sultanov.hello;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class UDPUtils {
    public static void waitForTermination(final ExecutorService pool, final long terminationSeconds) {
        pool.shutdown();
        try {
            if (!pool.awaitTermination(terminationSeconds, TimeUnit.SECONDS)) {
                System.err.println("Timeout occurred while waiting for thread pool termination");
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.err.printf("Interrupted exception occurred: %s%n", e.getMessage());
        }
    }
}