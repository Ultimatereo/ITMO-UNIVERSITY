package info.kgeorgiy.ja.sultanov.student;

import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class StudentDB implements StudentQuery {
    private final static Comparator<Student> nameComparator = Comparator
            .comparing(Student::getLastName)
            .thenComparing(Student::getFirstName)
            .reversed()
            .thenComparing(Comparator.naturalOrder());

    private static <T, R, S> S getCollect(
            final Collection<T> collection,
            final Function<? super T, R> mapper,
            final Collector<R, ?, S> collector) {
        return collection.stream()
                .map(mapper)
                .collect(collector);
    }

    private static <R> List<R> getCollectList(
            final Collection<Student> students,
            final Function<? super Student, R> mapper) {
        return getCollect(students, mapper, Collectors.toList());
    }

    private static <R> Set<R> getCollectSet(
            final Collection<Student> students,
            final Function<? super Student, R> mapper) {
        return getCollect(students, mapper, Collectors.toCollection(TreeSet::new));
    }

    private static List<Student> sortStudent(
            final Collection<Student> students,
            final Comparator<? super Student> comparator) {
        return students
                .stream()
                .sorted(comparator)
                .collect(Collectors.toList());
    }

    private static List<Student> findStudent(
            final Collection<Student> students,
            Predicate<? super Student> predicate) {
        return students
                .stream()
                .filter(predicate)
                .sorted(nameComparator)
                .collect(Collectors.toList());
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return getCollectList(students, Student::getFirstName);
    }


    @Override
    public List<String> getLastNames(List<Student> students) {
        return getCollectList(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return getCollectList(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return getCollectList(students, st -> st.getFirstName() + " " + st.getLastName());
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return getCollectSet(students, Student::getFirstName);
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream()
                .max(Comparator.naturalOrder())
                .map(Student::getFirstName)
                .orElse("");
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortStudent(students, Comparator.naturalOrder());
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortStudent(students, nameComparator);
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findStudent(students, st -> st.getFirstName().equals(name));
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findStudent(students, st -> st.getLastName().equals(name));
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return findStudent(students, st -> st.getGroup().equals(group));
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return findStudentsByGroup(students, group).stream()
                .collect(Collectors.toMap(
                        Student::getLastName,
                        Student::getFirstName,
                        BinaryOperator.minBy(Comparator.naturalOrder())
                ));
    }
}
