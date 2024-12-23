from aiohttp import web, ClientSession, TCPConnector
from yarl import URL


async def proxy_handler(request: web.Request) -> web.Response:
    """
    Check request contains http url in query args:
        /fetch?url=http%3A%2F%2Fexample.com%2F
    and trying to fetch it and return body with http status.
    If url passed without scheme or is invalid raise 400 Bad request.
    On failure raise 502 Bad gateway.
    :param request: aiohttp.web.Request to handle
    :return: aiohttp.web.Response
    """
    url_param = request.query.get('url')

    if not url_param:
        return web.Response(status=400, text='No url to fetch')

    try:
        url = URL(url_param)
    except ValueError:
        return web.Response(status=400, text='Bad Request. Invalid URL.')

    if not url.scheme:
        return web.Response(status=400, text='Empty url scheme')

    if url.scheme != "http" and url.scheme != "https":
        return web.Response(status=400, text=f'Bad url scheme: {url.scheme}')

    async with request.app['session'].get(url) as resp:
        return web.Response(status=resp.status, body=await resp.read())


async def setup_application(app: web.Application) -> None:
    """
    Setup application routes and aiohttp session for fetching
    :param app: app to apply settings with
    """
    app.router.add_route('GET', '/fetch', proxy_handler)

    # Create aiohttp session with TCPConnector to reuse connections
    connector = TCPConnector(limit=0)
    session = ClientSession(connector=connector)

    app['session'] = session


async def teardown_application(app: web.Application) -> None:
    """
    Application with aiohttp session for tearing down
    :param app: app for tearing down
    """
    await app['session'].close()


if __name__ == '__main__':
    app = web.Application()
    app.on_startup.append(setup_application)
    app.on_cleanup.append(teardown_application)

    web.run_app(app)
