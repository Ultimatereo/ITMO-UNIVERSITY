from typing import List


def normalize_path(path: str) -> str:
    """
    :param path: unix path to normalize
    :return: normalized path
    """
    if not path or path == '.':
        return "."

    stack: List[str] = []
    is_absolute = path[0] == "/"
    components = path.split("/")

    for component in components:
        if component == "..":
            if stack and stack[-1] != "..":
                stack.pop()
            elif not stack and is_absolute:
                continue
            else:
                stack.append(component)
        elif component and component != ".":
            stack.append(component)

    normalized_path = "/".join(stack)

    if is_absolute:
        normalized_path = "/" + normalized_path
    if not normalized_path:
        return '.'
    return normalized_path
