import sys
import traceback
from contextlib import contextmanager
from typing import Iterator, TextIO, Type


@contextmanager
def supresser(*types_: Type[BaseException]) -> Iterator[None]:
    try:
        yield
    except types_:
        pass


@contextmanager
def retyper(type_from: Type[BaseException], type_to: Type[BaseException]) -> Iterator[None]:
    try:
        yield
    except type_from as exc:
        new_exc = type_to(*exc.args)
        new_exc.__cause__ = exc
        raise new_exc from None


@contextmanager
def dumper(stream: TextIO | None = None) -> Iterator[None]:
    if stream is None:
        stream = sys.stderr

    try:
        yield
    except Exception as exc:
        exc_type, exc_value, exc_traceback = sys.exc_info()
        formatted_exception = traceback.format_exception_only(exc_type, exc_value)
        stream.write(''.join(formatted_exception))
        stream.flush()
        raise exc from None
