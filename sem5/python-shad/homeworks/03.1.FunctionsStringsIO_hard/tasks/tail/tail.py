import sys
import typing as tp
from pathlib import Path


def tail(filename: Path, lines_amount: int = 10, output: tp.IO[bytes] | None = None) -> None:
    """
    :param filename: file to read lines from (the file can be very large)
    :param lines_amount: number of lines to read
    :param output: stream to write requested amount of last lines from file
                   (if nothing specified stdout will be used)
    """
    lines_wanted = lines_amount + 1
    f = open(filename, "rb")
    BLOCK_SIZE = 128
    f.seek(0, 2)
    block_end_byte = f.tell()
    lines_to_go = lines_wanted
    block_number = -1
    blocks = []
    while lines_to_go > 0 and block_end_byte > 0:
        if block_end_byte - BLOCK_SIZE > 0:
            f.seek(block_number * BLOCK_SIZE, 2)
            blocks.append(f.read(BLOCK_SIZE))
        else:
            f.seek(0, 0)
            blocks.append(f.read(block_end_byte))
        lines_found = blocks[-1].count(b'\n')
        lines_to_go -= lines_found
        block_end_byte -= BLOCK_SIZE
        block_number -= 1
    f.close()
    all_read_text = b''.join(reversed(blocks))
    lines = all_read_text.splitlines()[-lines_wanted:]
    if output is None:
        output = sys.stdout.buffer
    start = 0
    if len(lines) == lines_amount + 1:
        start = 1
    for i in range(start, len(lines)):
        line = lines[i]
        output.write(line)
        output.write(b'\n')
