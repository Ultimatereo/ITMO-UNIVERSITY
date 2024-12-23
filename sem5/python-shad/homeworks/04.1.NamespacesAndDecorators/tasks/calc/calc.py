import sys
from typing import Any

PROMPT = '>>> '


def run_calc(context: dict[str, Any] | None = None) -> None:
    """Run interactive calculator session in the specified namespace"""
    if context is None:
        context = {}
    context['__builtins__'] = {}
    for line in sys.stdin:
        user_input = line.strip()

        if not user_input:
            break

        print(PROMPT, end='')
        result = eval(user_input, context)

        print(result)
    print(PROMPT)