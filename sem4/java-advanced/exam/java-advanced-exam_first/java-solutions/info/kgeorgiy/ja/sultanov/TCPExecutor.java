package info.kgeorgiy.ja.sultanov;

import java.io.*;
import java.net.*;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class TCPExecutor {

    // В main указываем количество тредов
    public static void main(String[] args) {
        if (Objects.isNull(args) || args.length != 2 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Error: Expected 2 non-null argument specifying the number of threads.");
            return;
        }
        String file = args[0];
        int threads;
        try {
            threads = Integer.parseInt(args[1]);
        } catch (NumberFormatException e) {
            System.err.println("Error: The argument should be an integer specifying the number of threads.");
            return;
        }

        ExecutorService executor = Executors.newFixedThreadPool(threads); // Создаем пул потоков

        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            // Читаем файл заданий
            String line;
            while ((line = reader.readLine()) != null) {
                String[] taskData = line.split(" ", 4);
                TCPContext context = TCPContext.parse(taskData);
                if (!Objects.isNull(context)) {
                    executor.execute(() -> executeTask(context)); // Выполняем задание в отдельном потоке
                }
            }
        } catch (FileNotFoundException e) {
            System.err.println("Error: Tasks file not found.");
            e.printStackTrace();
        } catch (IOException e) {
            System.err.println("Error: Failed to read tasks file.");
            e.printStackTrace();
        } finally {
            executor.shutdown(); // Завершаем пул потоков после выполнения всех заданий
        }
    }

    private static void executeTask(TCPContext context) {
        try {
            long currentTime = System.currentTimeMillis();
            long delay = context.time() - currentTime;
            if (delay > 0) {
                Thread.sleep(delay); // Ожидаем до указанного времени суток
            }
            try (Socket socket = new Socket(InetAddress.getByName(context.host()), context.port())) {
                final DataOutput dos = new DataOutputStream(socket.getOutputStream());
                dos.writeUTF(context.query());
                socket.shutdownOutput();

                final DataInput dis = new DataInputStream(socket.getInputStream());
                final String response = dis.readUTF();
                System.out.println("Response from " + context.host() + ":" + context.port() + " - " + response);
            } catch (IOException e) {
                System.err.println("Error: An I/O error occurred during task execution: " + e.getMessage());
                e.printStackTrace();
            }
        } catch (InterruptedException e) {
            System.err.println("Error: The task execution was interrupted.");
            e.printStackTrace();
            Thread.currentThread().interrupt();
        }
    }
}
