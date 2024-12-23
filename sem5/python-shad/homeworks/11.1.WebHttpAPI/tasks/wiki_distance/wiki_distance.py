from pathlib import Path
from bs4 import BeautifulSoup
from urllib.parse import unquote

# Directory to save your .json files to
# NB: create this directory if it doesn't exist
SAVED_JSON_DIR = Path(__file__).parent / 'visited_paths'
TESTDATA_DIR = Path(__file__).parent / 'testdata'


def _distance(source_file: str, target_file: str, visited_urls: set[str], dist: int) -> int | None:
    with open(TESTDATA_DIR / source_file, "r", encoding="utf-8") as source:
        page: str = '\n'.join(source.readlines())
    soup: BeautifulSoup = BeautifulSoup(page, 'lxml')
    block = soup.find("div", class_="mw-parser-output").find("p", recursive=False).findAll("a")
    for tag in block:
        if ('href' in tag.attrs and tag.attrs['href'].startswith("/wiki/") and "title" in tag.attrs and
                not tag.attrs["title"].startswith("Файл:")):
            out: str = unquote(tag.attrs['href'].split("/")[-1], encoding='utf-8')
            if out == target_file:
                return dist
            if out not in visited_urls:
                visited_urls.add(out)
                return _distance(out, target_file, visited_urls, dist + 1)
    return None


def distance(source_url: str, target_url: str) -> int | None:
    """Amount of wiki articles which should be visited to reach the target one
    starting from the source url. Assuming that the next article is choosing
    always as the very first link from the first article paragraph (tag <p>).
    If the article does not have any paragraph tags or any links in the first
    paragraph then the target is considered unreachable and None is returned.
    If the next link is pointing to the already visited article, it should be
    discarded in favor of the second link from this paragraph. And so on
    until the first not visited link will be found or no links left in paragraph.
    NB. The distance between neighbour articles (one is pointing out to the other)
    assumed to be equal to 1.
    :param source_url: the url of source article from wiki
    :param target_url: the url of target article from wiki
    :return: the distance calculated as described above
    """
    source: str = unquote(source_url.split("/")[-1], encoding='utf-8')
    target: str = unquote(target_url.split("/")[-1], encoding='utf-8')
    visited_urls: set[str] = set()
    visited_urls.add(source)
    return _distance(source, target, visited_urls, 1)
