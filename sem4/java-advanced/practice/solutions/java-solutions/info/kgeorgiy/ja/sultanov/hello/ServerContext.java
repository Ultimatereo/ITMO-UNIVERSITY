package info.kgeorgiy.ja.sultanov.hello;

import java.util.Arrays;
import java.util.Objects;

public record ServerContext(int port, int threads) {
    public static ServerContext parse(String[] args) {
        if (Objects.isNull(args) || args.length != 2 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Error: Expected 2 non-null arguments.");
            return null;
        }
        final int port = parseToInteger(args[0], "port");
        final int threads = parseToInteger(args[1], "threads");
        if (port == Integer.MIN_VALUE || threads == Integer.MIN_VALUE) {
            return null;
        }
        return new ServerContext(port, threads);
    }

    private static int parseToInteger(String arg, String parameter) {
        try {
            return Integer.parseInt(arg);
        } catch (NumberFormatException e) {
            System.err.println("Error: Invalid argument format. Expected an integer for the '" + parameter + "' parameter.");
            return Integer.MIN_VALUE;
        }
    }
}
