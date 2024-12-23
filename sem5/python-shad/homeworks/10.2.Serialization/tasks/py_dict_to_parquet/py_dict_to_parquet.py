import pyarrow as pa
import pyarrow.parquet as pq


ValueType = int | list[int] | str | dict[str, str]


def save_rows_to_parquet(rows: list[dict[str, ValueType]], output_filepath: str) -> None:
    """
    Save rows to parquet file.

    :param rows: list of rows containing data.
    :param output_filepath: local filepath for the resulting parquet file.
    :return: None.
    """
