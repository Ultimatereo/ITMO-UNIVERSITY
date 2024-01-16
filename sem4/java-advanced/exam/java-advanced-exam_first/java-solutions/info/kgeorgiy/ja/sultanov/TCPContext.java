package info.kgeorgiy.ja.sultanov;

import java.util.Arrays;
import java.util.Objects;

public record TCPContext(long time, String host, int port, String query) {
    public static TCPContext parse(String[] args) {
        if (Objects.isNull(args) || args.length != 4 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Error: Expected 4 non-null arguments. Got: " + Arrays.toString(args));
            return null;
        }
        final long time = parseToLong(args[0], "time");
        final String host = args[1];
        final int port = parseToInteger(args[2], "port");
        final String query = args[3];

        if (port == Integer.MIN_VALUE || time == Integer.MIN_VALUE) {
            return null;
        }
        return new TCPContext(time, host, port, query);
    }

    private static int parseToInteger(String arg, String parameter) {
        try {
            return Integer.parseInt(arg);
        } catch (NumberFormatException e) {
            System.err.println("Error: Invalid argument format. Expected an integer for the '" + parameter + "' parameter. Got: " + arg);
            return Integer.MIN_VALUE;
        }
    }

    private static long parseToLong(String arg, String parameter) {
        try {
            return Long.parseLong(arg);
        } catch (NumberFormatException e) {
            System.err.println("Error: Invalid argument format. Expected a long for the '" + parameter + "' parameter. Got: " + arg);
            return Long.MIN_VALUE;
        }
    }
}
