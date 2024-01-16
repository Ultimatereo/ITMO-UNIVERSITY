package info.kgeorgiy.ja.sultanov;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class TodoList {
    private final HashMap<Integer, TodoItem> todoMap = new HashMap<>();
    private final Set<Integer> completeTasks = new HashSet<>();
    private final Set<Integer> incompleteTasks = new HashSet<>();

    public void addTask(TodoItem todoItem) {
        todoMap.put(todoItem.getId(), todoItem);
        if (todoItem.isDone()) {
            completeTasks.add(todoItem.getId());
        } else {
            incompleteTasks.add(todoItem.getId());
        }
    }

    public boolean deleteTask(int id) {
        if (!todoMap.containsKey(id)) {
            return false;
        }
        if (todoMap.get(id).isDone()) {
            completeTasks.remove(id);
        } else {
            incompleteTasks.remove(id);
        }
        todoMap.remove(id);
        return true;
    }

    public boolean markDone(int id) {
        if (!todoMap.containsKey(id)) {
            return false;
        }
        incompleteTasks.remove(id);
        completeTasks.add(id);
        todoMap.get(id).setDone();
        return true;
    }

    public Iterable<TodoItem> getAllTasks() {
        return todoMap.values();
    }

    public List<TodoItem> getCompletedTasks() {
        return getFilterTasks(completeTasks);
    }

    public List<TodoItem> getIncompleteTasks() {
        return getFilterTasks(incompleteTasks);
    }

    private List<TodoItem> getFilterTasks(Set<Integer> filterSet) {
        return filterSet.stream().map(todoMap::get).collect(Collectors.toList());
    }
}
