from datetime import datetime
from zoneinfo import ZoneInfo

DEFAULT_TZ_NAME = "Europe/Moscow"
DEFAULT_TZ_INFO = ZoneInfo(DEFAULT_TZ_NAME)


def use_default_tz(num_args_to_convert):  # type: ignore
    def decorator(func):  # type: ignore
        def wrapper(*args, **kwargs):  # type: ignore
            converted_args = []
            for i, dt in enumerate(args):
                if i < num_args_to_convert and isinstance(dt, datetime):
                    if dt.tzinfo is None:
                        dt = dt.replace(tzinfo=DEFAULT_TZ_INFO)
                    else:
                        dt = dt.astimezone(DEFAULT_TZ_INFO)
                converted_args.append(dt)
            return func(*converted_args, **kwargs)

        return wrapper

    return decorator


def ensure_default_tz(func):  # type: ignore
    def wrapper(*args, **kwargs):  # type: ignore
        result = func(*args, **kwargs)
        if result.tzinfo is None:
            result = result.replace(tzinfo=DEFAULT_TZ_INFO)
        else:
            result = result.astimezone(DEFAULT_TZ_INFO)
        return result

    return wrapper


def now() -> datetime:
    """Return now in default timezone"""
    return datetime.now(tz=DEFAULT_TZ_INFO)


@use_default_tz(1)
def strftime(dt: datetime, fmt: str) -> str:
    """Return dt converted to string according to format in default timezone"""
    return dt.strftime(fmt)


@ensure_default_tz
def strptime(dt_str: str, fmt: str) -> datetime:
    """Return dt parsed from string according to format in default timezone"""
    return datetime.strptime(dt_str, fmt)


@use_default_tz(2)
def diff(first_dt: datetime, second_dt: datetime) -> int:
    """Return seconds between two datetimes rounded down to closest int"""
    return int((second_dt - first_dt).total_seconds())


@use_default_tz(1)
def timestamp(dt: datetime) -> int:
    """Return timestamp for given datetime rounded down to closest int"""
    return int(dt.timestamp())


def from_timestamp(ts: float) -> datetime:
    """Return datetime from given timestamp"""
    return datetime.fromtimestamp(ts, tz=DEFAULT_TZ_INFO)
