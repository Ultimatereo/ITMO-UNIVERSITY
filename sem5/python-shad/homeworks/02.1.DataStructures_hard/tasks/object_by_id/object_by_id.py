import ctypes
import struct
import typing as tp

LONG_LEN = 8
INT_LEN = 4
CHAR_LEN = 1

ULONG_CHAR = "L" if ctypes.sizeof(ctypes.c_ulong) == 8 else "Q"
LONG_CHAR = "l" if ctypes.sizeof(ctypes.c_long) == 8 else "q"
INT_TYPE_LINK = struct.unpack(ULONG_CHAR + ULONG_CHAR, ctypes.string_at(id(100), 16))[1]
BOOL_TYPE_LINK = struct.unpack(ULONG_CHAR + ULONG_CHAR, ctypes.string_at(id(True), 16))[1]
FLOAT_TYPE_LINK = struct.unpack(ULONG_CHAR + ULONG_CHAR, ctypes.string_at(id(10.4), 16))[1]
STR_TYPE_LINK = struct.unpack(ULONG_CHAR + ULONG_CHAR, ctypes.string_at(id("aboba"), 16))[1]
LIST_TYPE_LINK = struct.unpack(ULONG_CHAR + ULONG_CHAR, ctypes.string_at(id([2, 3]), 16))[1]
TUPLE_TYPE_LINK = struct.unpack(ULONG_CHAR + ULONG_CHAR, ctypes.string_at(id((2, 3)), 16))[1]

id_to_object: tp.Dict[int, tp.Any] = {}


def get_object_by_id(object_id: int) -> int | float | tuple[tp.Any, ...] | list[tp.Any] | str | bool:
    """
    Restores object by its id.
    :param object_id: Object ID.
    :return: An object that corresponds to object_id.
    """
    # print(struct.unpack("LLliii", ctypes.string_at(object_id, 36)))
    global id_to_object
    counter_of_links, type_link = struct.unpack(ULONG_CHAR + ULONG_CHAR, ctypes.string_at(object_id, 16))
    if type_link == INT_TYPE_LINK:
        size = struct.unpack(LONG_CHAR, ctypes.string_at(object_id + 16, 8))[0]
        sign = 1 if size > 0 else -1
        size = abs(size)
        value = 0
        e = 2 ** 30
        for i in range(size):
            part = struct.unpack('i', ctypes.string_at(object_id + 24 + i * 4, 4))[0]
            value += e ** i * part
        return value * sign
    if type_link == BOOL_TYPE_LINK:
        return bool(struct.unpack('i', ctypes.string_at(object_id + 24, 4))[0])
    if type_link == FLOAT_TYPE_LINK:
        return struct.unpack("d", ctypes.string_at(object_id + 16, 8))[0]
    if type_link == STR_TYPE_LINK:
        line = b""
        size = struct.unpack(ULONG_CHAR, ctypes.string_at(object_id + 16, 8))[0]
        for i in range(size):
            b = struct.unpack("s", ctypes.string_at(object_id + 48 + i, 1))[0]
            if b == 0:
                return str(line)
            line += b
        return line.decode("utf-8")
    if type_link == LIST_TYPE_LINK:
        array: tp.List[tp.Any] = list()
        data_1 = struct.unpack("5" + ULONG_CHAR, ctypes.string_at(object_id, 40))
        size = data_1[2]
        data_link = data_1[3]
        data_2 = struct.unpack(f"{size}" + ULONG_CHAR, ctypes.string_at(data_link, size * 8))
        id_to_object[object_id] = array
        for i in range(size):
            if data_2[i] in id_to_object:
                array.append(id_to_object[data_2[i]])
            else:
                data = get_object_by_id(data_2[i])
                array.append(data)
                id_to_object[data_2[i]] = data
        return array
    if type_link == TUPLE_TYPE_LINK:
        size = struct.unpack(ULONG_CHAR, ctypes.string_at(object_id + 16, 8))[0]
        elements = struct.unpack(f"{size}" + ULONG_CHAR, ctypes.string_at(object_id + 24, 8 * size))
        t: tp.List[tp.Any] = list()
        for el in elements:
            if el in id_to_object:
                t.append(id_to_object[el])
            else:
                data = get_object_by_id(el)
                t.append(data)
                id_to_object[el] = data
        return tuple(t)
    assert False, "Unreachable code"
