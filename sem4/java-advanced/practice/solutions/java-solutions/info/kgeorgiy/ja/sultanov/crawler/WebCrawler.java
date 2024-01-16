package info.kgeorgiy.ja.sultanov.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;


public class WebCrawler implements Crawler {
    private final ExecutorService downloaders;
    private final Downloader downloader;
    private final ExecutorService extractors;

    public WebCrawler(
            final Downloader downloader,
            final int downloaders,
            final int extractors,
            final int ignoredPerHost
    ) {
        this.downloader = downloader;
        this.downloaders = Executors.newFixedThreadPool(downloaders);
        this.extractors = Executors.newFixedThreadPool(extractors);
    }

    public static void main(String[] args) {
        //:NOTE: args.length > 5
        if (args == null || args.length < 1 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Not enough arguments are provided. You should provide " +
                    "url [depth [downloads [extractors [perHost]]]]");
        } else {
            int[] arguments = new int[4];
            for (int i = 0; i < 4; i++) {
                arguments[i] = i < args.length ? Integer.parseInt(args[i + 1]) : 1;
            }
            try (final Crawler crawler = new WebCrawler(
                    new CachingDownloader(2),
                    arguments[1],
                    arguments[2],
                    arguments[3]
            )) {
                crawler.download(args[0], arguments[0]);
            } catch (final NumberFormatException e) {
                System.err.printf("Expected numbers in arguments but found: %s!\n", e.getMessage());
            } catch (final IOException e) {
                System.err.printf("Exception while initializing CachingDownloader: %s!\n", e.getMessage());
            }
        }
    }

    private void download(final String url, final int depth, final Queue<String> queue,
                          final Phaser phaser, final Set<String> downloaded, final Map<String, IOException> errors,
                          final Set<String> cached) {
        try {
            URLUtils.getHost(url);
            phaser.register();
            downloaders.submit(downloadersPoolAction(url, queue, cached, downloaded, errors, phaser, depth));
        } catch (final MalformedURLException e) {
            errors.put(url, e);
        }
    }

    private Runnable downloadersPoolAction(final String url, final Queue<String> queue,
                                           final Set<String> cached, final Set<String> downloaded,
                                           final Map<String, IOException> errors,
                                           final Phaser phaser, int depth) {
        return () -> {
            try {
                final var doc = downloader.download(url);
                downloaded.add(url);
                if (depth > 1) {
                    phaser.register();
                    extractors.submit(extractorsPoolAction(doc, url, queue, cached, phaser));
                }
            } catch (final IOException e) {
                errors.put(url, e);
            } finally {
                phaser.arriveAndDeregister();
            }
        };
    }

    private Runnable extractorsPoolAction(final Document doc, final String url, final Queue<String> queue,
                                          final Set<String> cached, final Phaser phaser) {
        return () -> {
            try {
                final List<String> links = doc.extractLinks();
                final List<String> uncachedLinks = links.stream()
                        .filter(cached::add)
                        .toList();
                queue.addAll(uncachedLinks);
            } catch (final IOException e) {
                System.err.println("Exception when extracting links from " + url + ".");
            } finally {
                phaser.arriveAndDeregister();
            }
        };
    }

    //:NOTE: Для того, чтобы не добавлять методам большое число одинаковых параметров,
    //:NOTE: удобно выделить отдельный внутренний класс с полями, содержащими эти параметры,
    //:NOTE: и создавать отдельный экземпляр класса для каждого запуска download.
    @Override
    public Result download(final String url, final int depth) {
        final Map<String, IOException> errors = new ConcurrentHashMap<>();
        final Set<String> downloaded = ConcurrentHashMap.newKeySet();
        final Set<String> cached = ConcurrentHashMap.newKeySet();
        final Phaser phaser = new Phaser(1);
        cached.add(url);
        final Queue<String> prevUrls = new ConcurrentLinkedDeque<>();
        final Queue<String> curUrls = new ConcurrentLinkedDeque<>();
        prevUrls.add(url);
        for (int i = 0; i < depth; i++) {
            for (final String prevUrl : prevUrls) {
                download(prevUrl, depth - i, curUrls, phaser, downloaded, errors, cached);
            }
            phaser.arriveAndAwaitAdvance();
            prevUrls.clear();
            prevUrls.addAll(curUrls);
            curUrls.clear();
        }
        return new Result(new ArrayList<>(downloaded), errors);
    }

    @Override
    public void close() {
        extractors.shutdown();
        downloaders.shutdown();
        try {
            if (!(extractors.awaitTermination(1, TimeUnit.MINUTES) & downloaders.awaitTermination(1, TimeUnit.MINUTES))) {
                System.err.println("Still waiting for executors to shut down...");
            }
        } catch (final InterruptedException e) {
            Thread.currentThread().interrupt();
            System.err.println("Could not shutdown pools");
        }
    }
}
