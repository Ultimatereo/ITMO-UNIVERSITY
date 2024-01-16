package info.kgeorgiy.ja.sultanov.hello;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public final class HelloUDPClient extends AbstractClient {
    private static final long REQUEST_TIMEOUT = 6L;

    public static void main(String[] args) {
        ClientContext context = ClientContext.parse(args);
        if (context != null) {
            new HelloUDPClient().run(context);
        }
    }

    protected void run(ClientContext context) {
        try {
            final InetSocketAddress socketAddress = new InetSocketAddress(InetAddress.getByName(context.host()), context.port());
            final ExecutorService pool = Executors.newFixedThreadPool(context.threads());
            for (int i = 0; i < context.threads(); i++) {
                final int thread = i;
                pool.submit(() -> sendMessages(context, socketAddress, thread));
            }
            waitForTermination(pool, REQUEST_TIMEOUT * context.threads() * context.requests());
        } catch (final UnknownHostException e) {
            System.err.printf("Unknown host: %s%n", context.host());
        } catch (final SecurityException e) {
            System.err.println("Security manager exception occurred");
        }
    }

    private void sendMessages(ClientContext context, SocketAddress socketAddress, int thread) {
        try (final DatagramSocket datagramSocket = new DatagramSocket()) {
            datagramSocket.setSoTimeout(SOCKET_TIMEOUT);
            final int bufferSize = datagramSocket.getSendBufferSize();
            final DatagramPacket packet = new DatagramPacket(new byte[bufferSize], bufferSize, socketAddress);
            int id = 0;
            while (id < context.requests()) {
                final String message = context.createRequestMessage(thread, id);
                sendMessage(message, datagramSocket, packet, bufferSize);
                id++;
            }
        } catch (final SocketException e) {
            System.err.printf("Socket exception occurred for host %s on port %d: %s%n", context.host(), context.port(),
                    e.getMessage());
        }
    }

    private void sendMessage(
            final String message,
            final DatagramSocket socket,
            final DatagramPacket packet,
            final int bufferSize
    ) {
        System.out.printf("Sending message: %s%n", message);
        boolean receivedResponse = false;
        while (!receivedResponse) {
            try {
                packet.setData(message.getBytes(StandardCharsets.UTF_8));
                socket.send(packet);
                packet.setData(new byte[bufferSize]);
                socket.receive(packet);
                String responseMessage = new String(packet.getData(), packet.getOffset(), packet.getLength(), StandardCharsets.UTF_8);
                if (responseMessage.contains(message)) {
                    System.out.printf("Received response: %s%n", responseMessage);
                    receivedResponse = true;
                }
            } catch (final IOException e) {
                System.err.printf("Send or receive exception occurred for message: %s%n%s%n", message, e.getMessage());
            }
        }
    }

    private void waitForTermination(final ExecutorService pool, final long terminationSeconds) {
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
