package info.kgeorgiy.ja.sultanov;

import java.io.*;
import java.util.ResourceBundle;
import java.util.Scanner;

public class TodoListManager {
    private static final String FILENAME = "todo.txt";
    private static final String DELIMITER = "###";
    private static final String MESSAGES_FILE = "info.kgeorgiy.ja.sultanov/messages";
    private final TodoList todoList;
    private final ResourceBundle messages;
    private int lastId = 0;

    public TodoListManager() {
        messages = ResourceBundle.getBundle(MESSAGES_FILE);
        todoList = new TodoList();
        loadTodoListFromFile();
    }

    public static void main(String[] args) {
        TodoListManager todoListManager = new TodoListManager();
        todoListManager.run();
    }

    private void loadTodoListFromFile() {
        // :NOTE: * сканнер медленно
        try (Scanner scanner = new Scanner(new File(FILENAME))) {
            int maxId = Integer.MIN_VALUE;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] parts = line.split(DELIMITER);
                // :NOTE: * обработка ошибок
                try {
                    int id = Integer.parseInt(parts[0]);
                    String name = parts[1];
                    boolean isDone = Boolean.parseBoolean(parts[2]);

                    TodoItem todoItem = new TodoItem(id, name, isDone);
                    todoList.addTask(todoItem);
                    maxId = Math.max(maxId, id);
                } catch (NumberFormatException e) {
                    // Если в неправильном формате задана строка, то просто игнорируем её
                }
            }
            lastId = maxId;
        } catch (FileNotFoundException e) {
            // Если файл не найден, игнорируем ошибку, так как список задач будет пустым.
        }
    }

    private void saveTodoListToFile() {
        // :NOTE: * PrinterWriter плохой класс не кидает эксепшены
        try (PrintWriter writer = new PrintWriter(new FileWriter(FILENAME))) {
            for (TodoItem todoItem : todoList.getAllTasks()) {
                String line = todoItem.getId() + DELIMITER +
                        todoItem.getTodoItemName() + DELIMITER +
                        todoItem.isDone();
                writer.println(line);
            }
        } catch (IOException e) {
            System.out.println(messages.getString("saveError"));
        }
    }

    private void printMenu() {
        System.out.println(messages.getString("menu.choice.prompt"));
        System.out.println("1. " + messages.getString("menu.choice.viewAllTasks"));
        System.out.println("2. " + messages.getString("menu.choice.viewCompletedTasks"));
        System.out.println("3. " + messages.getString("menu.choice.viewIncompleteTasks"));
        System.out.println("4. " + messages.getString("menu.choice.addTask"));
        System.out.println("5. " + messages.getString("menu.choice.deleteTask"));
        System.out.println("6. " + messages.getString("menu.choice.markTaskAsDone"));
        System.out.println("0. " + messages.getString("menu.choice.exit"));
    }

    private void printTodoItem(TodoItem todoItem) {
        String status = todoItem.isDone() ? messages.getString("task.status.done") :
                messages.getString("task.status.notDone");
        System.out.println(todoItem.getId() + ". " + status + " " + todoItem.getTodoItemName());
    }

    private void printTodoList(Iterable<TodoItem> todoList) {
        if (!todoList.iterator().hasNext()) {
            System.out.println(messages.getString("taskList.empty"));
            return;
        }

        for (TodoItem todoItem : todoList) {
            printTodoItem(todoItem);
        }
    }

    private void viewAllTasks() {
        System.out.println(messages.getString("taskList.title"));
        printTodoList(todoList.getAllTasks());
    }

    private void viewCompletedTasks() {
        System.out.println(messages.getString("taskList.completed"));
        printTodoList(todoList.getCompletedTasks());
    }

    private void viewIncompleteTasks() {
        System.out.println(messages.getString("taskList.incomplete"));
        printTodoList(todoList.getIncompleteTasks());
    }

    private void addTask() {
        System.out.println(messages.getString("task.add.prompt"));
        Scanner scanner = new Scanner(System.in);
        String taskName = scanner.nextLine();

        int taskId = getNextId();
        TodoItem todoItem = new TodoItem(taskId, taskName);
        todoList.addTask(todoItem);
        saveTodoListToFile();
        System.out.println(messages.getString("task.add.success"));
        viewAllTasks();
    }

    private int getNextId() {
        lastId += 1;
        return lastId;
    }

    private void deleteTask() {
        viewAllTasks();
        System.out.println(messages.getString("task.delete.prompt"));
        Scanner scanner = new Scanner(System.in);
        int taskId = scanner.nextInt();

        if (todoList.deleteTask(taskId)) {
            saveTodoListToFile();
            System.out.println(messages.getString("task.delete.success"));
            viewAllTasks();
        } else {
            System.out.println(messages.getString("task.delete.notFound"));
        }
    }

    private void markTaskAsDone() {
        viewAllTasks();
        System.out.println(messages.getString("task.markAsDone.prompt"));
        Scanner scanner = new Scanner(System.in);
        int taskId = scanner.nextInt();
        if (todoList.markDone(taskId)) {
            saveTodoListToFile();
            System.out.println(messages.getString("task.markAsDone.success"));
            viewAllTasks();
        } else {
            System.out.println(messages.getString("task.markAsDone.notFound"));
        }
    }

    public void run() {
        Scanner scanner = new Scanner(System.in);

        int choice = -1;
        while (choice != 0) {
            printMenu();
            choice = scanner.nextInt();
            scanner.nextLine(); // Очищаем буфер после считывания числа

            switch (choice) {
                case 0 -> System.out.println(messages.getString("bye"));
                case 1 -> viewAllTasks();
                case 2 -> viewCompletedTasks();
                case 3 -> viewIncompleteTasks();
                case 4 -> addTask();
                case 5 -> deleteTask();
                case 6 -> markTaskAsDone();
                default -> System.out.println(messages.getString("menu.error"));
            }
        }
    }
}

