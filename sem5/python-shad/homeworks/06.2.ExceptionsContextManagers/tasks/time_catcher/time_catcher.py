import time
import typing as tp
from types import TracebackType


class TimeoutException(Exception):
    pass


class SoftTimeoutException(TimeoutException):
    def __str__(self) -> str:
        return "Soft timeout!"


class HardTimeoutException(TimeoutException):
    def __str__(self) -> str:
        return "Hard timeout!"


class TimeCatcher:

    def __init__(self, soft_timeout: float | None = None, hard_timeout: float | None = None):
        assert soft_timeout is None or soft_timeout > 0
        assert hard_timeout is None or hard_timeout > 0
        assert soft_timeout is None or hard_timeout is None or soft_timeout <= hard_timeout
        self.soft_timeout = soft_timeout
        self.hard_timeout = hard_timeout
        self.start_time: float | None = None

    def __str__(self) -> str:
        return f"Time consumed: {float(self)}"

    def __float__(self) -> float:
        if self.start_time is None:
            return self.time
        return time.time() - self.start_time + 0.0000000000001

    def __enter__(self) -> tp.Self:
        self.start_time = time.time()
        return self

    def __exit__(self,
                 exc_type: type[BaseException] | None,
                 exc_val: BaseException | None,
                 exc_tb: TracebackType | None) -> None:
        self.time = float(self)
        print(f"{self.time} seconds has passed!")
        self.start_time = None
        if self.soft_timeout and self.time > self.soft_timeout:
            raise SoftTimeoutException
        if self.hard_timeout and self.time > self.hard_timeout:
            raise HardTimeoutException
