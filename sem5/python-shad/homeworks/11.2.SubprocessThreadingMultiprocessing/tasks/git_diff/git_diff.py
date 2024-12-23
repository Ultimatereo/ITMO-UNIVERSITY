import os
import subprocess
from pathlib import Path


def get_changed_dirs(git_path: Path, from_commit_hash: str, to_commit_hash: str) -> set[Path]:
    """
    Get directories which content was changed between two specified commits
    :param git_path: path to git repo directory
    :param from_commit_hash: hash of commit to do diff from
    :param to_commit_hash: hash of commit to do diff to
    :return: sequence of changed directories between specified commits
    """
    # Перейти в директорию с git репозиторием
    os.chdir(git_path)

    # Выполнить git diff между коммитами
    diff_output = subprocess.run(
        ["git", "diff", "--name-only", from_commit_hash, to_commit_hash],
        stdout=subprocess.PIPE,
        text=True
    ).stdout

    # Разбить вывод git diff по строкам
    changed_files = diff_output.strip().split('\n')

    # Получить директории из путей к измененным файлам
    changed_dirs = {Path(file).parent.absolute() for file in changed_files}

    return changed_dirs
