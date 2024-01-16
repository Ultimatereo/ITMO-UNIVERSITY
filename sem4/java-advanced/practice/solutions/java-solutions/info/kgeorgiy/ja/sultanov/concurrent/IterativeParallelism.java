package info.kgeorgiy.ja.sultanov.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class IterativeParallelism implements ScalarIP {
    private <T, U> U apply(int threads,
                           List<? extends T> values,
                           Function<Stream<? extends T>, ? extends U> function,
                           Function<Stream<? extends U>, ? extends U> collector) throws InterruptedException {
        final List<Stream<? extends T>> valuesForThreads = splitValuesForThreads(values, threads);
        threads = valuesForThreads.size();
        ArrayList<Thread> threadsList = new ArrayList<>();
        final List<U> res = new ArrayList<>(Collections.nCopies(threads, null));
        for (int i = 0; i < threads; i++) {
            final int index = i;
            Thread thread = new Thread(() -> res.set(index, function.apply(valuesForThreads.get(index))));
            threadsList.add(thread);
            thread.start();
        }
        for (Thread thread : threadsList) {
            thread.join();
        }
        return collector.apply(res.stream());
    }

    private <T> List<Stream<? extends T>> splitValuesForThreads(List<? extends T> values, int threads) {
        ArrayList<Stream<? extends T>> splitValues = new ArrayList<>();
        threads = Math.min(threads, values.size());
        int chunkSize = values.size() / threads;
        int remainder = values.size() % threads;
        int fromIndex = 0;
        for (int i = 0; i < threads; i++) {
            splitValues.add(
                    values.subList(
                            fromIndex,
                            fromIndex + chunkSize + (remainder > 0 ? 1 : 0)
                    ).stream());
            fromIndex = fromIndex + chunkSize + (remainder > 0 ? 1 : 0);
            remainder--;
        }
        return splitValues;
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return apply(
                threads,
                values,
                s -> s.max(comparator).orElse(null),
                s -> s.max(comparator).orElse(null));
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, comparator.reversed());
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return apply(
                threads,
                values,
                s -> s.allMatch(predicate),
                s -> s.allMatch(b -> b));
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return !all(threads, values, predicate.negate());
    }

    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return apply(
                threads,
                values,
                s -> (int) s.filter(predicate).count(),
                s -> s.mapToInt(Integer::intValue).sum()
        );
    }
}