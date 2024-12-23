import java.io.IOException;

public class DotToPngConverter {
    public static void convertDotToPng(String dotFilePath, String pngFilePath) {
        try {
            // Command to run Graphviz dot tool
            String command = "C:\\Program Files\\Graphviz\\bin\\dot -Tpng " + dotFilePath + " -o " + pngFilePath;

            ProcessBuilder processBuilder = new ProcessBuilder(command.split(" "));
            processBuilder.redirectErrorStream(true); // Redirect error stream to output stream

            Process process = processBuilder.start();

            // Wait for the process to complete
            int exitCode = process.waitFor();
//            System.out.println("Exit Code: " + exitCode);

        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}
