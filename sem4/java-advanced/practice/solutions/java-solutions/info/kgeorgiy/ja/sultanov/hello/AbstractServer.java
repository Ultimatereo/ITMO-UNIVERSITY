package info.kgeorgiy.ja.sultanov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

public abstract class AbstractServer implements HelloServer {
    protected static final long THREAD_TERMINATION_SECONDS = Long.MAX_VALUE;

    @Override
    public void start(int port, int threads) {
        start(new ServerContext(port, threads));
    }

    protected abstract void start(ServerContext context);

    protected String createResponse(String message) {
        return "Hello, " + message;
    }

}
