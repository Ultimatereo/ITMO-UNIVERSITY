import zlib
from collections import defaultdict
from dataclasses import dataclass
from enum import Enum
from pathlib import Path


class BlobType(Enum):
    """Helper class for holding blob type"""
    COMMIT = b'commit'
    TREE = b'tree'
    DATA = b'blob'

    @classmethod
    def from_bytes(cls, type_: bytes) -> 'BlobType':
        for member in cls:
            if member.value == type_:
                return member
        assert False, f'Unknown type {type_.decode("utf-8")}'


@dataclass
class Blob:
    """Any blob holder"""
    type_: BlobType
    content: bytes


@dataclass
class Commit:
    """Commit blob holder"""
    tree_hash: str
    parents: list[str]
    author: str
    committer: str
    message: str


@dataclass
class Tree:
    """Tree blob holder"""
    children: dict[str, Blob]


def read_blob(path: Path) -> Blob:
    """
    Read blob-file, decompress and parse header
    :param path: path to blob-file
    :return: blob-file type and content
    """

    with open(path, 'rb') as file:
        data = file.read()
        data = zlib.decompress(data)

    # Split header and content
    header, content = data.split(b'\0', 1)

    # Parse the blob type from the header
    blob_type = BlobType.from_bytes(header.split(b' ')[0])

    return Blob(type_=blob_type, content=content)


def traverse_objects(obj_dir: Path) -> dict[str, Blob]:
    """
    Traverse directory with git objects and load them
    :param obj_dir: path to git "objects" directory
    :return: mapping from hash to blob with every blob found
    """
    objects = {}

    for obj_file in obj_dir.rglob('*'):
        if obj_file.is_file():
            blob = read_blob(obj_file)
            objects[obj_file.parent.stem + obj_file.stem] = blob

    return objects


def parse_commit(blob: Blob) -> Commit:
    """
    Parse commit git blob
    :param blob: git blob with commit type
    :return: parsed commit
    """
    content = blob.content.decode('utf-8')
    lines = content.split('\n')
    d = defaultdict(list)
    last_key_index = -1
    for i in range(len(lines)):
        line = lines[i]
        if line == "":
            last_key_index = i
            break
        index = line.find(' ')
        key = line[:index]
        value = line[index + 1:]
        d[key].append(value)
    assert last_key_index != -1
    message = lines[last_key_index + 1]
    for i in range(last_key_index + 2, len(lines) - 1):
        message += '\n'
        message += lines[i]
    return Commit(d['tree'][0], d['parent'], d['author'][0], d['committer'][0], message)


def parse_tree(blobs: dict[str, Blob], tree_root: Blob, ignore_missing: bool = True) -> Tree:
    """
    Parse tree blob
    :param blobs: all read blobs (by traverse_objects)
    :param tree_root: tree blob to parse
    :param ignore_missing: ignore blobs which were not found in objects directory
    :return: tree contains children blobs (or only part of them found in objects directory)
    NB. Children blobs are not being parsed according to type.
        Also nested tree blobs are not being traversed.
    """
    content = tree_root.content
    children = {}

    while content:
        space_idx = content.index(b' ')
        null_idx = content.index(b'\0')

        content[:space_idx]
        name = content[space_idx + 1:null_idx].decode('utf-8')
        hash_hex = content[null_idx + 1:null_idx + 21]
        content = content[null_idx + 21:]

        hash = hash_hex.hex()

        if hash in blobs:
            children[name] = blobs[hash]
        elif not ignore_missing:
            raise ValueError(f"Blob with hash {hash} not found in the repository.")

    return Tree(children)


def find_initial_commit(blobs: dict[str, Blob]) -> Commit:
    """
    Iterate over blobs and find initial commit (without parents)
    :param blobs: blobs read from objects dir
    :return: initial commit
    """
    for blob in blobs.values():
        if blob.type_ == BlobType.COMMIT:
            commit = parse_commit(blob)
            if not commit.parents:
                return commit
    assert False, "Unreachable code"


def search_file(blobs: dict[str, Blob], tree_root: Blob, filename: str) -> Blob:
    """
    Traverse tree blob (can have nested tree blobs) and find requested file,
    check if file was not found (assertion).
    :param blobs: blobs read from objects dir
    :param tree_root: root blob for traversal
    :param filename: requested file
    :return: requested file blob
    """
    tree = parse_tree(blobs, tree_root)

    def find_file(tree: Tree, filename: str) -> Blob:
        if filename in tree.children:
            return tree.children[filename]
        for child in tree.children.values():
            if child.type_ == BlobType.TREE:
                result = find_file(parse_tree(blobs, child), filename)
                if result:
                    return result
        assert False, "Unreachable code"

    file_blob = find_file(tree, filename)
    assert file_blob, f"File '{filename}' not found in the repository."

    return file_blob
