import typing as tp


def reformat_git_log(inp: tp.IO[str], out: tp.IO[str]) -> None:
    """Reads git log from `inp` stream, reformats it and prints to `out` stream

    Expected input format: `<sha-1>\t<date>\t<author>\t<email>\t<message>`
    Output format: `<first 7 symbols of sha-1>.....<message>`
    """
    for line in inp:
        sha1, date, author, email, message = line.split('\t')
        out.write(sha1[:7])
        out.write('.' * (80 - 6 - len(message)))
        out.write(message)
