package info.kgeorgiy.ja.sultanov.walk;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;


public class Walk {
    private static final MessageDigest SHA_DIGEST;
    private static final String DEFAULT_HASH = "0000000000000000000000000000000000000000000000000000000000000000";

    static {
        try {
            SHA_DIGEST = MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) {
        if (args == null) {
            System.err.println("Args is null!");
        } else if (args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("You have to put two non-null arguments! " +
                    "The first one for inputPath and the second one for outputPath!");
        } else {
            walkAndWriteHashCode(args[0], args[1]);
        }
    }

    private static void walkAndWriteHashCode(String inputString, String outputString) {
        try {
            Path inputPath = Paths.get(inputString);
            try {
                Path outputPath = Paths.get(outputString);
                walkAndWriteHashCode(inputPath, outputPath);
            } catch (InvalidPathException e) {
                System.err.println("Invalid output path error path appeared! " + e.getMessage());
            }
        } catch (InvalidPathException e) {
            System.err.println("Invalid input path error appeared! " + e.getMessage());
        }
    }

    private static void walkAndWriteHashCode(Path inputPath, Path outputPath) {
        try (BufferedReader reader = Files.newBufferedReader(inputPath)) {
            try {
                if (outputPath.getParent() != null) {
                    Files.createDirectories(outputPath.getParent());
                }
            } catch (IOException e) {
                System.err.println("Exception appeared when creating directories! " + e.getMessage());
            }
            try (BufferedWriter writer = Files.newBufferedWriter(outputPath)) {
                String filePathString;
                while ((filePathString = reader.readLine()) != null) {
                    try {
                        File file = Paths.get(filePathString).toFile();
                        writer.write(getFileSHA256(file) + " " + filePathString + "\n");
                    } catch (Exception e) {
                        writer.write(DEFAULT_HASH + " " + filePathString + "\n");
                    }
                }
            }
        } catch (FileNotFoundException | NoSuchFileException e) {
            System.err.println("File is not found! " + e.getMessage());
        } catch (AccessDeniedException e) {
            System.err.println("Access denied to file! " + e.getMessage());
        } catch (IOException e) {
            System.err.println("IOException appeared! " + e.getMessage());
        }
    }

    private static String getFileSHA256(File file) {
        return getFileChecksum(SHA_DIGEST, file);
    }

    private static String getFileChecksum(MessageDigest digest, File file) {
        try (FileInputStream fis = new FileInputStream(file)) {
            byte[] byteArray = new byte[1024];
            int bytesCount;
            while ((bytesCount = fis.read(byteArray)) != -1) {
                digest.update(byteArray, 0, bytesCount);
            }
            byte[] bytes = digest.digest();
            StringBuilder sb = new StringBuilder();
            for (byte aByte : bytes) {
                sb.append(Integer.toString((aByte & 0xff) + 0x100, 16).substring(1));
            }
            return sb.toString();
        } catch (IOException e) {
            return DEFAULT_HASH;
        }
    }
}
