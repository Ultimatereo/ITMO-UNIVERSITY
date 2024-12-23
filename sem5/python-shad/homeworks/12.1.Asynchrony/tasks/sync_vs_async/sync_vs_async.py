import asyncio
import concurrent.futures
from typing import Any

import aiohttp
import requests


async def async_fetch(session: aiohttp.ClientSession, url: str) -> str:
    """
    Asynchronously fetch (get-request) single url using provided session
    :param session: aiohttp session object
    :param url: target http url
    :return: fetched text
    """
    async with session.get(url) as response:
        return await response.text()


async def async_requests(urls: list[str]) -> list[Any]:
    """
    Concurrently fetch provided urls using aiohttp
    :param urls: list of http urls to fetch
    :return: list of fetched texts
    """
    async with aiohttp.ClientSession() as session:
        tasks = [async_fetch(session, url) for url in urls]
        return await asyncio.gather(*tasks)


def sync_fetch(session: requests.Session, url: str) -> str:
    """
    Synchronously fetch (get-request) single url using provided session
    :param session: requests session object
    :param url: target http url
    :return: fetched text
    """
    with session.get(url) as response:
        return response.text


def threaded_requests(urls: list[str]) -> list[str]:
    """
    Concurrently fetch provided urls with requests in different threads
    :param urls: list of http urls to fetch
    :return: list of fetched texts
    """
    with concurrent.futures.ThreadPoolExecutor() as executor:
        with requests.Session() as session:
            # Use executor.map to asynchronously fetch URLs
            results = list(executor.map(sync_fetch, [session] * len(urls), urls))
            return results


if __name__ == '__main__':
    async_urls = ['http://example.com', 'http://example.org', 'http://example.net']
    sync_urls = ['http://example.com', 'http://example.org', 'http://example.net']

    # Asynchronous requests
    async_results = asyncio.run(async_requests(async_urls))
    print("Asynchronous results:", async_results)

    # Synchronous requests
    sync_results = threaded_requests(sync_urls)
    print("Synchronous (threaded) results:", sync_results)
