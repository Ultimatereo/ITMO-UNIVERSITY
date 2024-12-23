import dis
import types


def count_operations(source_code: types.CodeType) -> dict[str, int]:
    """Count byte code operations in given source code.

    :param source_code: the bytecode operation names to be extracted from
    :return: operation counts
    """
    operation_counts: dict[str, int] = {}

    def count_operations_recursive(code: types.CodeType) -> None:
        for instruction in dis.get_instructions(code):
            op_name = instruction.opname
            operation_counts[op_name] = operation_counts.get(op_name, 0) + 1

            if instruction.argval and isinstance(instruction.argval, types.CodeType):
                count_operations_recursive(instruction.argval)

    count_operations_recursive(source_code)

    return operation_counts
