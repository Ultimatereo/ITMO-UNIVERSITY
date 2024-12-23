def count_util(text: str, flags: str | None = None) -> dict[str, int]:
    """
    :param text: text to count entities
    :param flags: flags in command-like format - can be:
        * -m stands for counting characters
        * -l stands for counting lines
        * -L stands for getting length of the longest line
        * -w stands for counting words
    More than one flag can be passed at the same time, for example:
        * "-l -m"
        * "-lLw"
    Ommiting flags or passing empty string is equivalent to "-mlLw"
    :return: mapping from string keys to corresponding counter, where
    keys are selected according to the received flags:
        * "chars" - amount of characters
        * "lines" - amount of lines
        * "longest_line" - the longest line length
        * "words" - amount of words
    """
    result = {}

    if flags is None:
        chars, lines, longest_line, words = True, True, True, True
    else:
        chars = flags.find("m") != -1
        lines = flags.find("l") != -1
        longest_line = flags.find("L") != -1
        words = flags.find("w") != -1
        if not (chars or lines or longest_line or words):
            chars, lines, longest_line, words = True, True, True, True

    if chars:
        result['chars'] = len(text)

    if lines:
        result['lines'] = text.count("\n")

    if longest_line:
        _lines = text.splitlines()
        _lines.append("")
        result['longest_line'] = max(len(_line) for _line in _lines)

    if words:
        result['words'] = len(text.split())

    return result
