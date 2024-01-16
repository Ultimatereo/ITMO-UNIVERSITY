package info.kgeorgiy.ja.sultanov.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public final class HelloUDPServer extends AbstractServer {
    private DatagramSocket socket;
    private ExecutorService main;
    private ExecutorService pool;

    public static void main(String[] args) {
        ServerContext context = ServerContext.parse(args);
        if (context != null) {
            try (final HelloUDPServer server = new HelloUDPServer()) {
                server.start(context);
                new Scanner(System.in).nextLine();
            }
        }
    }

    protected void start(ServerContext context) {
        try {
            socket = new DatagramSocket(context.port());
            pool = Executors.newFixedThreadPool(context.threads());
            main = Executors.newSingleThreadExecutor();
            main.execute(() -> {
                try {
                    final int bufferSize = socket.getReceiveBufferSize();
                    while (!Thread.interrupted()) {
                        final DatagramPacket packet = new DatagramPacket(new byte[bufferSize], bufferSize);
                        socket.receive(packet);

                        pool.execute(() -> processPacket(packet, context.port()));
                    }
                } catch (final IOException e) {
                    System.err.println("IO exception on port: " + context.port() + "\n" + e.getMessage());
                }
            });
        } catch (final SocketException e) {
            System.err.println("Socket exception on port: " + context.port() + "\n" + e.getMessage());
        } catch (final SecurityException e) {
            System.err.println("Security exception on port: " + context.port() + "\n" + e.getMessage());
        }
    }

    private void processPacket(DatagramPacket packet, int port) {
        String request = new String(packet.getData(), packet.getOffset(), packet.getLength(), StandardCharsets.UTF_8);
        String response = createResponse(request);
        byte[] responseData = response.getBytes(StandardCharsets.UTF_8);

        packet.setData(responseData);
        try {
            socket.send(packet);
        } catch (final IOException e) {
            System.err.println("IO exception on port: " + port + "\n" + e.getMessage());
        }
    }

    @Override
    public void close() {
        socket.close();
        UDPUtils.waitForTermination(main, THREAD_TERMINATION_SECONDS);
        UDPUtils.waitForTermination(pool, THREAD_TERMINATION_SECONDS);
    }
}
