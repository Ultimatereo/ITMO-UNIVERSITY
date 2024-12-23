import click

from .decode import decode_message
from .encode import encode_message
from .utils import Path, get_base_file, write_file, read_file


@click.group()
def cli() -> None:
    """Base module command"""
    pass


@cli.command()
@click.argument('output_filename', type=click.Path(exists=False, dir_okay=False, path_type=Path))
@click.argument('message', type=str)
def encode(output_filename: Path, message: str) -> None:
    data = get_base_file()
    data = encode_message(data, message)
    write_file(data, output_filename)


@cli.command()
@click.argument('input_filename', type=click.Path(exists=True, dir_okay=False, path_type=Path))
def decode(input_filename: Path) -> None:
    data = read_file(input_filename)
    message = decode_message(data)
    print(message)
