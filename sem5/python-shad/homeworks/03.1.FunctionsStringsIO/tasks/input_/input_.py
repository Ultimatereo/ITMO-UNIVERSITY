import sys
import typing as tp


def input_(prompt: str | None = None,
           inp: tp.IO[str] | None = None,
           out: tp.IO[str] | None = None) -> str | None:
    """Read a string from `inp` stream. The trailing newline is stripped.

    The `prompt` string, if given, is printed to `out` stream without a
    trailing newline before reading input.

    If the user hits EOF (*nix: Ctrl-D, Windows: Ctrl-Z+Return), return None.

    `inp` and `out` arguments are optional and should default to `sys.stdin`
    and `sys.stdout` respectively.
    """
    if not inp:
        inp = sys.stdin
    if not out:
        out = sys.stdout

    if prompt:
        out.write(prompt)
        out.flush()
    try:
        line = inp.readline().split('\n')
        if not line[0]:
            return None
        return line[0]
    except KeyboardInterrupt:
        return None
