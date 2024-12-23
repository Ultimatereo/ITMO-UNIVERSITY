import json
import os
import typing as tp
from collections import defaultdict

import requests
import xlsxwriter
from bs4 import BeautifulSoup

"""
CIAN_PARSER

This code is here to help you parse spb.cian.ru.
"""

PAGES = 50
BASE_LINK_FOR_PAGES = "https://spb.cian.ru/cat.php/" \
                      "?deal_type=sale&engine_version=2&object_type%5B0%5D=2&offer_type=flat&region=2"


def get_pages_html(base_link: str = BASE_LINK_FOR_PAGES, pages: int = PAGES) -> None:
    """
    Downloads needed number of pages from base_link to the directory `pages`.
    Also needs `assets/request_page.har` for correct work.

    Args:
        base_link (str, optional): The base link from which pages are downloaded.
            It should support getting pages by adding p attribute. Defaults to BASE_LINK_FOR_PAGES.
        pages (int, optional): Number of pages to download. Defaults to PAGES.
    """

    with open("assets/request_page.har", "r", encoding="utf-8") as json_file:
        request_page_har = json.load(json_file)

    request: dict[str, tp.Any] = request_page_har["log"]["entries"][0]["request"]
    print(request)
    for p in range(1, pages + 1):
        link = f"{base_link}&p={p}"
        response: str = requests.get(link, request).text

        with open(f"pages/{p}.html", "w", encoding="utf-8") as file:
            file.write(response)


def parse_pages() -> None:
    """Parses the pages from `pages` directory and then saves all the links to flats in `assets\flat_links.txt`"""
    for p in range(1, PAGES + 1):
        with open(f"pages/{p}.html", "r", encoding="utf-8") as file:
            page: str = 'n'.join(file.readlines())
        soup: BeautifulSoup = BeautifulSoup(page, 'lxml')
        block = soup.findAll("a", class_="_93444fe79c--media--9P6wN")
        with open("assets/flat_links.txt", "a") as file:
            for tag in block:
                file.write(tag.attrs['href'])
                file.write("\n")


def getText(obj):
    if obj is None:
        return None
    return obj.text


def getNum(s: str) -> int:
    num = ""
    for i in s:
        if i.isnumeric():
            num += i
    return int(num)


def parse_flat(filepath: str, link: str) -> dict[str, tp.Any]:
    """
    Parses html file from given filepath and creates a dictionary with all attributes.

    Args:
         filepath (str) : A local filepath where html file that we have to parse is located.
         link (str): URL link of the flat.

    Returns:
         Dictionary with all attributes of a flat.
    """
    data: dict[str, tp.Any] = defaultdict(None)
    with open(filepath, "r", encoding="utf-8") as file:
        flat: str = 'n'.join(file.readlines())
    soup: BeautifulSoup = BeautifulSoup(flat, 'lxml')
    data['Ссылка на страницу'] = link
    data['Локальное место'] = filepath
    data['Название'] = getText(soup.find("h1", class_="a10a3f92e9--title--vlZwT"))
    data['ЖК'] = getText(soup.find("a", class_="a10a3f92e9--link--A5SdC"))
    addressObjects = soup.findAll("a", class_="a10a3f92e9--address--SMU25")
    if (obj := soup.find("button", class_="a10a3f92e9--button--lyQVM")) is not None:
        data['Просмотры'] = getNum(obj.contents[1])
    if (obj := soup.find("div", class_="a10a3f92e9--amount--ON6i1")) is not None:
        data['Стоимость'] = getNum(obj.contents[0].contents[0])
    if len(addressObjects) != 0:
        data['Адрес'] = ' '.join([getText(el) for el in addressObjects])
    undergroundObjects = soup.findAll("a", class_="a10a3f92e9--underground_link--VnUVj")
    if len(undergroundObjects) != 0:
        data['Метро'] = ', '.join([getText(el) for el in undergroundObjects])
    for soupObj in soup.findAll("div", class_="a10a3f92e9--text--eplgM"):
        data[soupObj.contents[0].text] = getText(soupObj.contents[1])
    for soupObj in soup.findAll("div", class_="a10a3f92e9--item--qJhdR"):
        data[soupObj.contents[0].text] = getText(soupObj.contents[1])
    print(data)
    return data


def get_flats_html() -> None:
    """
    Downloads all html files corresponding to flats from the `assets\flat_links`
    and saves them into directory `flats`.
    """
    with open("assets/request_page.har", "r", encoding="utf-8") as json_file:
        request_flat_har = json.load(json_file)
    request: dict[str, tp.Any] = request_flat_har["log"]["entries"][0]["request"]
    with open("assets/flat_links.txt", "r") as flat_links:
        for link in flat_links:
            response = requests.get(link.strip(), request).text
            with open(f"flats/{link}.html", "w", encoding="utf-8") as file:
                file.write(response)


def parse_flats(filename_to_link: dict[str, str]) -> None:
    """
    Parses all html files corresponding to flats from the directory `flats` and creates `cian_spb.xlsx` file.

    Args:
         filename_to_link (dict[str, str]): A dictionary of filenames of flats to its url links.
    """
    directory_path = "flats"
    xlsx_file_path = "cian_spb.xlsx"
    columns = [
        "Название",
        "Ссылка на страницу",
        "Локальное место",
        "Просмотры",
        "Стоимость",
        "ЖК",
        "Адрес",
        "Метро",
        "Общая площадь",
        "Жилая площадь",
        "Площадь кухни",
        "Этаж",
        "Год сдачи",
        "Отделка",
        "Тип жилья",
        "Высота потолков",
        "Санузел",
        "Вид из окон",
        "Количество лифтов",
        "Тип дома",
        "Парковка"
    ]
    if os.path.exists(directory_path) and os.path.isdir(directory_path):
        files = os.listdir(directory_path)
        with xlsxwriter.Workbook(xlsx_file_path) as xlsx_file:
            worksheet = xlsx_file.add_worksheet("Квартиры СПБ")
            worksheet.write_row(0, 0, columns)
            for i, file in enumerate(files):
                file_path = os.path.join(directory_path, file)
                if os.path.isfile(file_path):
                    print(f"Processed {i + 1} flats")
                    data = parse_flat(file_path, filename_to_link[file_path])
                    worksheet.write_row(i + 1, 0, [data.get(key) for key in columns])


def get_filename_to_link() -> dict[str, str]:
    """
    Get a dictionary of filenames of saved flats to its url links

    Returns:
         A dictionary of filenames of flats to its url links.
    """
    f = 1
    filename_to_link = {}
    with open("assets/flat_links.txt", "r") as file:
        for link in file:
            filename_to_link[f"flats\\{f}.html"] = link.strip()
            f += 1
    return filename_to_link


if __name__ == '__main__':
    get_pages_html()
    parse_pages()
    get_flats_html()
    parse_flats(get_filename_to_link())
