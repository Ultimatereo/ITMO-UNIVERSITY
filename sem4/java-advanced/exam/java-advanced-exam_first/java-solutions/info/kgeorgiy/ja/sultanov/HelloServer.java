package info.kgeorgiy.ja.sultanov;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

public class HelloServer {
    private final static int SERVER_PORT = 12345;

    public static void main(String[] args) throws IOException {
        try (ServerSocket ssocket = new ServerSocket(SERVER_PORT)) {
            while (true) {
                try (Socket socket = ssocket.accept()) {
                    DataInput dis = new DataInputStream(socket.getInputStream());
                    String user = dis.readUTF();

                    System.out.println(user);

                    DataOutput dos = new DataOutputStream(socket.getOutputStream());
                    dos.writeUTF("Hello, " + user + "!");
                }
            }
        }
    }
}
