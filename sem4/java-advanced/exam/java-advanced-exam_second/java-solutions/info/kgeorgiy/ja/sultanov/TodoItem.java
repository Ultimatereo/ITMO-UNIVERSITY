package info.kgeorgiy.ja.sultanov;

public class TodoItem {
    private final int id;
    private final String todoItemName;
    private boolean isDone;

    public TodoItem(int id, String todoItemName, boolean isDone) {
        this.id = id;
        this.todoItemName = todoItemName;
        this.isDone = isDone;
    }

    public TodoItem(int id, String todoItemName) {
        this(id, todoItemName, false);
    }

    public String getTodoItemName() {
        return todoItemName;
    }

    public boolean isDone() {
        return isDone;
    }

    public void setDone(boolean done) {
        isDone = done;
    }

    public void setDone() {
        setDone(true);
    }

    public int getId() {
        return id;
    }
}
