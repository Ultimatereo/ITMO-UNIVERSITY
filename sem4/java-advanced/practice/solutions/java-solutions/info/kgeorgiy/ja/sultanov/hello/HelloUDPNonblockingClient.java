package info.kgeorgiy.ja.sultanov.hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

public final class HelloUDPNonblockingClient extends AbstractClient {
    public static void main(final String... args) {
        ClientContext context = ClientContext.parse(args);
        if (context != null) {
            new HelloUDPNonblockingClient().run(context);
        }
    }

    public void run(ClientContext context) {
        ArrayList<DatagramChannel> channels = new ArrayList<>();
        try (final Selector selector = Selector.open()) {
            InetSocketAddress socket = new InetSocketAddress(context.host(), context.port());
            for (int i = 0; i < context.threads(); i++) {
                DatagramChannel channel = DatagramChannel.open();
                channel.configureBlocking(false);
                channel.connect(socket);
                channel.register(selector, SelectionKey.OP_WRITE, new MetaInfo(i, channel.socket().getReceiveBufferSize()));
                channels.add(channel);
            }

            while (!selector.keys().isEmpty() && !Thread.interrupted()) {
                selector.select(SOCKET_TIMEOUT);
                final Set<SelectionKey> selectedKeys = selector.selectedKeys();
                if (selectedKeys.isEmpty()) {
                    for (SelectionKey key : selector.keys()) {
                        key.interestOps(SelectionKey.OP_WRITE);
                    }
                } else {
                    for (final Iterator<SelectionKey> it = selectedKeys.iterator(); it.hasNext(); ) {
                        final SelectionKey key = it.next();
                        try {
                            if (key.isValid()) {
                                final DatagramChannel channel = (DatagramChannel) key.channel();
                                final MetaInfo metaInfo = (MetaInfo) key.attachment();
                                if (key.isReadable()) {
                                    final ByteBuffer byteBuffer = metaInfo.byteBuffer;
                                    byteBuffer.clear();
                                    channel.receive(byteBuffer);
                                    byteBuffer.flip();
                                    final String response = StandardCharsets.UTF_8.decode(byteBuffer).toString();
                                    final String expectedResponse = context.createRequestMessage(metaInfo.threadId, metaInfo.requestId);
                                    if (response.contains(expectedResponse)) {
                                        System.out.printf("Received: %s%n", response);
                                        metaInfo.requestId++;
                                    }
                                    key.interestOps(SelectionKey.OP_WRITE);
                                    if (metaInfo.requestId >= context.requests()) {
                                        channel.close();
                                    }
                                } else {
                                    final String message = context.createRequestMessage(metaInfo.threadId, metaInfo.requestId);
                                    System.out.printf("Sending: %s%n", message);
                                    channel.send(ByteBuffer.wrap(message.getBytes(StandardCharsets.UTF_8)), socket);
                                    key.interestOps(SelectionKey.OP_READ);
                                }
                            }
                        } finally {
                            it.remove();
                        }
                    }
                }
            }
        } catch (final IOException e) {
            System.err.printf("An error occurred during communication on host: %s and port: %s%nException: %s%n",
                    context.host(), context.port(), e.getMessage());
        } finally {
            for (DatagramChannel channel : channels) {
                try {
                    channel.close();
                } catch (final IOException e) {
                    System.err.printf("An exception occurred while closing the channel: %s%nOn host: %s, port: %s%n",
                            e.getMessage(), context.host(), context.port());
                }
            }
        }
    }

    private static class MetaInfo {
        final int threadId;
        final ByteBuffer byteBuffer;
        int requestId;

        public MetaInfo(final int threadId, final int bufferSize) {
            this.threadId = threadId;
            this.byteBuffer = ByteBuffer.allocate(bufferSize);
        }
    }
}
