from pathlib import Path
import subprocess


def python_sort(file_in: Path, file_out: Path) -> None:
    """
    Sort tsv file using python built-in sort
    :param file_in: tsv file to read from
    :param file_out: tsv file to write to
    """
    with open(file_in, mode='r') as fin, open(file_out, mode='w') as fout:
        # Чтение строк из файла
        lines = fin.readlines()

        # Сортировка строк по заданному ключу
        sorted_lines = sorted(lines, key=lambda x: (int(x.split('\t')[1]), x.split('\t')[0]))

        # Запись отсортированных строк в файл
        fout.writelines(sorted_lines)


def util_sort(file_in: Path, file_out: Path) -> None:
    """
    Sort tsv file using sort util
    :param file_in: tsv file to read from
    :param file_out: tsv file to write to
    """
    # Вызов sort с использованием subprocess
    subprocess.run(["sort", "-o", str(file_out), "-k2,2n", "-k1,1", str(file_in)])
