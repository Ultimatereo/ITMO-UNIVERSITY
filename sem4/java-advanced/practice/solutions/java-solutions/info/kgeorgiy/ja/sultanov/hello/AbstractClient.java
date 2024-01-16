package info.kgeorgiy.ja.sultanov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

public abstract class AbstractClient implements HelloClient {
    protected static final int SOCKET_TIMEOUT = 228;

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        run(new ClientContext(host, port, prefix, threads, requests));
    }

    protected abstract void run(ClientContext context);
}
