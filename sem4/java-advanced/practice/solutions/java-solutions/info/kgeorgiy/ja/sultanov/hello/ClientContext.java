package info.kgeorgiy.ja.sultanov.hello;

import java.util.Arrays;
import java.util.Objects;

public record ClientContext(String host, int port, String prefix, int threads, int requests) {
    public static ClientContext parse(String[] args) {
        if (Objects.isNull(args) || args.length != 5 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Error: Expected 5 non-null arguments.");
            return null;
        }
        final String host = args[0];
        final String prefix = args[2];
        final int port = parseToInteger(args[1], "port");
        final int threads = parseToInteger(args[3], "threads");
        final int requests = parseToInteger(args[4], "requests");
        if (port == Integer.MIN_VALUE || threads == Integer.MIN_VALUE || requests == Integer.MIN_VALUE) {
            return null;
        }
        return new ClientContext(host, port, prefix, threads, requests);
    }

    private static int parseToInteger(String arg, String parameter) {
        try {
            return Integer.parseInt(arg);
        } catch (NumberFormatException e) {
            System.err.println("Error: Invalid argument format. Expected an integer for the '" + parameter + "' parameter.");
            return Integer.MIN_VALUE;
        }
    }

    public String createRequestMessage(int thread, int id) {
        return prefix + (thread + 1) + "_" + (id + 1);
    }
}
