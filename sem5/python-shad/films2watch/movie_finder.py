from os import getenv

import requests

owner_ids_file_path = "owner_ids.txt"


class MovieFinder:
    def __init__(self, vk_api_token: str, kp_api_token):
        self.kp_api_token = kp_api_token
        self.vk_api_token = vk_api_token
        self.base_vk_url = "https://api.vk.com"
        self.base_kp_url = "https://api.kinopoisk.dev"
        self.headers_vk = {
            "Authorization": f"Bearer {self.vk_api_token}",
            "Content-Type": "application/json"
        }
        self.headers_kp = {
            "X-API-KEY": self.kp_api_token,
            "accept": "application/json"
        }
        self.owner_ids = self.get_owner_ids()

    def search(self, query: str):
        """
        Method for finding minimal useful information about the film including the link to the film,
        its description and lots of different facts.

        :param query: Name of the film to be found.
        :return: link to the poster of the queried film, its description and basic info and link to the film itself.
        """
        url = f"{self.base_kp_url}/v1.4/movie/search"
        params = {
            'page': 1,
            'limit': 1,
            'query': query
        }
        response = requests.get(url, headers=self.headers_kp, params=params)

        response.raise_for_status()

        film_data = response.json()['docs'][0]

        film_name = film_data['name']
        film_description = film_data['shortDescription']
        film_genres = [f['name'] for f in film_data['genres']]
        film_year = film_data['year']
        film_countries = [f['name'] for f in film_data['countries']]
        film_poster_url = film_data['poster']['previewUrl']
        film_rating_kp = film_data['rating']['kp']
        film_rating_imdb = film_data['rating']['imdb']

        film_text = \
            f"""Название: {film_name}
Год производства: {film_year}
Страна: {', '.join(film_countries)}
Жанр: {', '.join(film_genres)}
Рейтинг на КиноПоиске: {film_rating_kp} / 10
Рейтинг на IMDB: {film_rating_imdb} / 10

Описание: {film_description}"""

        video_link = self.search_impl(film_name + " " + str(film_year), self.owner_ids)
        video_desc = None
        if video_link:
            video_desc = f"Фильм можно посмотреть вот [здесь]({video_link}) 😉"
        return film_poster_url, film_text, video_desc, film_name

    def search_impl(self, query: str, owner_ids: [int]):
        """
        Method for finding films according to given query and trusted owner_ids.

        :param query: Name of the film to be found.
        :param owner_ids: IDs of the vk contributors that have a good reputation (at least expected to be so).
        :return: Several vk video links (currently in this implementation just one).
        """
        for owner_id in owner_ids:
            r = self.find(query, owner_id)
            if r is None:
                continue
            return r
        return self.find(query)

    def find(self, query: str, owner_id: int = None) -> [str]:
        """
        Method for finding films according to given query and owner_id.

        :param query: Name of the film to be found.
        :param owner_id: ID of the vk contributor that have a good reputation (at least expected to be so).
        :return: Several vk video links (currently in this implementation just one).
        """

        params = {
            "q": query,
            "v": 5.199,
            "count": 1
        }
        if owner_id:
            params["owner_id"] = owner_id

        url = f"{self.base_vk_url}/method/video.search"
        try:
            response = requests.get(url, headers=self.headers_vk, params=params)

            response.raise_for_status()
            r = response.json()['response']['items']
            if len(r) == 0:
                return None
            return r[0]['player']
        except requests.exceptions.RequestException:
            return None

    # @staticmethod
    # def parse_vk_video_response(response: requests.Response):
    #     response_json = response.json()
    #     arr = []
    #     for item in response_json['response']['items']:
    #         arr.append(item['player'])
    #     return arr

    def get_owner_ids(self) -> [int]:
        """
        Method for initializing vk ids of trusted sources for videos.

        :return: array of ids of trusted sources on Vk.
        """
        with open(owner_ids_file_path, 'r') as f:
            lines = f.readlines()
        url = f"{self.base_vk_url}/method/utils.resolveScreenName"
        params = {
            "v": 5.199
        }
        owner_ids = []

        for line in lines:
            params["screen_name"] = line
            response = requests.get(url, headers=self.headers_vk, params=params).json()
            t = 1
            if response['response']['type'] == 'group':
                t = -1
            owner_ids.append(t * response['response']['object_id'])
        return owner_ids


def example(search_query: str) -> None:
    """
    Example code to show how MovieFinder is used.

    :param search_query: Name of the film to be found.
    """
    vk_token = getenv("VK_TOKEN")
    kp_token = getenv("KP_TOKEN")
    movie_finder = MovieFinder(vk_token, kp_token)

    result = movie_finder.search(search_query)

    if result:
        print(f"Search results for '{search_query}':")
        print('\n'.join(result))
    else:
        print("No results or an error occurred during the search.")


if __name__ == "__main__":
    # example("The Matrix")
    pass
