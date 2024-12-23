import datetime
import enum
from typing import List


class GranularityEnum(enum.Enum):
    """
    Enum for describing granularity
    """
    DAY = datetime.timedelta(days=1)
    TWELVE_HOURS = datetime.timedelta(hours=12)
    HOUR = datetime.timedelta(hours=1)
    THIRTY_MIN = datetime.timedelta(minutes=30)
    FIVE_MIN = datetime.timedelta(minutes=5)


def truncate_to_granularity(dt: datetime.datetime, gtd: GranularityEnum) -> datetime.datetime:
    """
    :param dt: datetime to truncate
    :param gtd: granularity
    :return: resulted datetime
    """
    # Отделяем день, месяц и год от времени
    date_part: datetime.date = dt.date()

    # Округляем только часы и минуты
    time_part: datetime.timedelta = datetime.timedelta(
        seconds=(dt.hour * 3600 + dt.minute * 60) // gtd.value.total_seconds() * gtd.value.total_seconds())

    # Объединяем отдельные части
    truncated_dt: datetime.datetime = datetime.datetime.combine(date_part, datetime.time()) + time_part

    return truncated_dt


class DtRange:
    def __init__(
            self,
            before: int,
            after: int,
            shift: int,
            gtd: GranularityEnum
    ) -> None:
        """
        :param before: number of datetimes should take before `given datetime`
        :param after: number of datetimes should take after `given datetime`
        :param shift: shift of `given datetime`
        :param gtd: granularity
        """
        self._before: int = before
        self._after: int = after
        self._shift: int = shift
        self._gtd: GranularityEnum = gtd

    def __call__(self, dt: datetime.datetime) -> List[datetime.datetime]:
        """
        :param dt: given datetime
        :return: list of datetimes in range
        """
        start_dt: datetime.datetime = truncate_to_granularity(dt + datetime.timedelta(hours=self._shift), self._gtd)
        return [start_dt + i * self._gtd.value for i in range(-self._before, self._after + 1)]


def get_interval(
        start_time: datetime.datetime,
        end_time: datetime.datetime,
        gtd: GranularityEnum
) -> List[datetime.datetime]:
    """
    :param start_time: start of interval
    :param end_time: end of interval
    :param gtd: granularity
    :return: list of datetimes according to granularity
    """
    truncated_start_time: datetime.datetime = truncate_to_granularity(start_time, gtd)

    answer: List[datetime.datetime] = []

    dt: datetime.datetime = truncated_start_time
    while dt <= end_time:
        if dt >= start_time:
            answer.append(dt)
        dt += gtd.value
    return answer
