package info.kgeorgiy.ja.sultanov.hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public final class HelloUDPNonblockingServer extends AbstractServer {

    private Selector selector;
    private DatagramChannel channel;
    private Queue<MetaInfo> responses;
    private int bufferSize;
    private ExecutorService pool;

    public static void main(final String... args) {
        ServerContext context = ServerContext.parse(args);
        if (context != null) {
            try (final HelloUDPNonblockingServer server = new HelloUDPNonblockingServer()) {
                server.start(context);
            }
        }
    }

    protected void start(ServerContext context) {
        try {
            selector = Selector.open();
            channel = DatagramChannel.open();
            channel.configureBlocking(false);
            bufferSize = channel.socket().getReceiveBufferSize();
            channel.register(selector, SelectionKey.OP_READ);
            channel.bind(new InetSocketAddress(context.port()));
            pool = Executors.newFixedThreadPool(context.threads());
            responses = new ConcurrentLinkedDeque<>();
            Executors.newSingleThreadExecutor().submit(this::startServer);
        } catch (IOException e) {
            System.err.println("Server setup error: " + e.getMessage() + ". On port: " + context.port());
        }
    }

    @Override
    public void close() {
        try {
            if (channel != null) channel.close();
            if (selector != null) selector.close();
            UDPUtils.waitForTermination(pool, THREAD_TERMINATION_SECONDS);
        } catch (IOException e) {
            System.err.printf("Exception occurred while closing resources: %s%n", e.getMessage());
        }
    }

    public void startServer() {
        while (!Thread.interrupted() && !channel.socket().isClosed()) {
            try {
                if (selector.select() > 0) {
                    for (final Iterator<SelectionKey> iter = selector.selectedKeys().iterator(); iter.hasNext(); ) {
                        final SelectionKey key = iter.next();
                        try {
                            if (key.isValid()) {
                                if (key.isReadable()) {
                                    read(key);
                                } else {
                                    write(key);
                                }
                            }
                        } finally {
                            iter.remove();
                        }
                    }
                }
            } catch (IOException e) {
                System.err.printf("Selector I/O exception: %s%n", e.getMessage());
                close();
            } catch (ClosedSelectorException e) {
                System.err.printf("Selector closed: %s%n", e.getMessage());
            }
        }
    }

    private void write(final SelectionKey key) {
        if (!responses.isEmpty()) {
            final MetaInfo metaInfo = responses.poll();
            final ByteBuffer buffer =
                    ByteBuffer.wrap(metaInfo.response.getBytes(StandardCharsets.UTF_8));
            try {
                channel.send(buffer, metaInfo.socket);
            } catch (IOException e) {
                System.err.printf("Write I/O exception: %s%n", e.getMessage());
            }
            key.interestOpsOr(SelectionKey.OP_READ);
        } else {
            key.interestOps(SelectionKey.OP_READ);
        }
    }

    private void read(final SelectionKey key) {
        try {
            final ByteBuffer buffer = ByteBuffer.allocate(bufferSize);
            final SocketAddress address = channel.receive(buffer);
            pool.submit(() -> {
                buffer.flip();
                final String receive = StandardCharsets.UTF_8.decode(buffer).toString();
                final String response = createResponse(receive);
                responses.add(new MetaInfo(address, response));
                key.interestOps(SelectionKey.OP_WRITE);
                selector.wakeup();
            });
        } catch (IOException e) {
            System.err.printf("Read I/O exception: %s%n", e.getMessage());
        }
    }

    private record MetaInfo(SocketAddress socket, String response) {
    }
}
