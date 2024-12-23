import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Tree {
    String node;
    List<Tree> children;

    public Tree(String node, Tree... children) {
        this.node = node;
        this.children = Arrays.asList(children);
    }

    public Tree(String node) {
        this.node = node;
        this.children = null; // Добавлено явное присвоение null для ясности
    }

    public void writeUsing(BufferedWriter writer) throws IOException {
        for (Tree child : children) {
            writer.write(node + "->" + child.node + ";\n");
            child.writeUsing(writer);
        }
    }
}
