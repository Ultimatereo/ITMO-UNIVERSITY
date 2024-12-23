import heapq
import typing as tp


def merge(input_streams: tp.Sequence[tp.IO[bytes]], output_stream: tp.IO[bytes]) -> None:
    """
    Merge input_streams in output_stream
    :param input_streams: list of input streams. Contains byte-strings separated by "\n". Nonempty stream ends with "\n"
    :param output_stream: output stream. Contains byte-strings separated by "\n". Nonempty stream ends with "\n"
    :return: None
    """
    # Создаем список для хранения пар (элемент, индекс потока)
    elements_with_index = []

    # Итерируемся по всем потокам и добавляем элементы в список
    for i, stream in enumerate(input_streams):
        element = stream.readline()
        if element:
            elements_with_index.append((int(element), i))

    # Превращаем список в мин-кучу
    heapq.heapify(elements_with_index)

    while elements_with_index:
        # Извлекаем минимальный элемент
        min_element, min_index = heapq.heappop(elements_with_index)

        # Записываем его в выходной поток
        output_stream.write(bytes(f"{min_element}\n", "utf8"))
        # Читаем следующий элемент из соответствующего потока и добавляем его в кучу
        next_element = input_streams[min_index].readline()
        if next_element:
            heapq.heappush(elements_with_index, (int(next_element), min_index))


if __name__ == "__main__":
    import io

    out = io.BytesIO()
    merge([io.BytesIO(b"1\n3\n5\n"), io.BytesIO(b"2\n4\n6\n")], out)
    print(out.getvalue())
