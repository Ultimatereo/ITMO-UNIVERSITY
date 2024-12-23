import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Tree {
    static int counter = 0;
    final String node;
    final int id;
    List<Tree> children;

    public Tree(String node, Tree... children) {
        this.node = node;
        this.children = Arrays.asList(children);
        this.id = counter++;
    }

    public Tree(String node) {
        this.node = node;
        this.children = new ArrayList<>();
        this.id = counter++;
    }

    // TODO: Extract this to another class cuz it's a bad design (Single Responsibility)
    @Override
    public String toString() {
        return "\"" + node + "_" + id + "\"";
    }

    public void writeUsing(BufferedWriter writer) throws IOException {
        writeNode(this.toString(), node, writer);
        for (Tree child : children) {
            writeNode(child.toString(), child.node, writer);
            writeEdge(this.toString(), child.toString(), writer);
            child.writeUsing(writer);
        }
    }

    private void writeEdge(String parent, String childNode, BufferedWriter writer) throws IOException {
        writer.write(parent + "->" + childNode + ";\n");
    }

    private void writeNode(String v_name, String label, BufferedWriter writer) throws IOException {
        writer.write(v_name + " [label=\"" + label + "\"]\n");
    }
}
