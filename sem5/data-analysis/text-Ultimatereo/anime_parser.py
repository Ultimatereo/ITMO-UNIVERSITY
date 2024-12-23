import json
import csv
import os
import time
from concurrent.futures import ThreadPoolExecutor

import pandas as pd
import requests
from tqdm import tqdm

USER_AGENT = "AnimeParser"
pd.options.display.width = None
pd.options.display.max_columns = None
pd.options.display.max_colwidth = None


def get_str_from_file(file_path):
    try:
        with open(file_path, "r") as file:
            token = file.read().strip()
            return token
    except Exception as e:
        print("Ошибка при чтении файла:", str(e))
        return None


token = json.loads(get_str_from_file("token.json"))
# print(token)
access_token = token['access_token']


def save_to_csv(data_dict_list, filename):
    # Получаем список всех уникальных ключей из словарей
    fieldnames = list(set().union(*(d.keys() for d in data_dict_list)))

    # Открываем CSV файл для записи
    with open(filename, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.DictWriter(file, fieldnames=fieldnames)

        # Записываем заголовки столбцов
        writer.writeheader()

        # Записываем данные
        for row in data_dict_list:
            writer.writerow(row)


def get_data_from_api(api_url, token, i):
    while True:
        try:
            headers = {"Authorization": f"Bearer {token}", "User-Agent": USER_AGENT}
            response = requests.get(api_url, headers=headers)
            # Проверяем успешность запроса
            if response.status_code == 200:
                # Если запрос успешен, возвращаем данные
                with open(f"pics/get_request_{i}.jpg", "bw") as file:
                    file.write(response.content)
                return None
            elif response.status_code == 429:
                continue
            else:
                # Если запрос неудачен, выводим сообщение об ошибке
                print("Ошибка при выполнении запроса:", response.status_code, "\n", response)
                return None
        except Exception as e:
            # Обрабатываем возможные исключения при выполнении запроса
            print("Ошибка при выполнении запроса:", str(e))
            return None


def get_token(authorization_code):
    url = "https://shikimori.one/oauth/token"
    headers = {
        "User-Agent": "AnimeParser"
    }
    data = {
        "grant_type": "authorization_code",
        "client_id": "hlAnIZVEj3b_pRHn_ZYpUuCSt2KpvyMTSRfc9U5YylE",
        "client_secret": "pkOSM1kdV7ZZVEAkfWgKtcwECsR7ShOgsMcpKklEh6w",
        "code": authorization_code,
        "redirect_uri": "urn:ietf:wg:oauth:2.0:oob"
    }

    response = requests.post(url, headers=headers, data=data)
    if response.status_code == 200:
        # Если запрос прошел успешно, сохраняем ответ в файл
        with open("token.json", "w") as file:
            file.write(response.text)
        print("Ответ сохранен в файл 'token.json'.")
    else:
        print("Ошибка при выполнении запроса:", response.status_code, "\n", response)


def merge_csv(input_files, output_file):
    # Загрузка первого CSV файла
    df1 = pd.read_csv(input_files[0])

    # Загрузка второго CSV файла
    df2 = pd.read_csv(input_files[1])

    # Слияние двух DataFrame'ов по общим столбцам
    merged_df = pd.merge(df1, df2, how='outer')

    # Сохранение слитого DataFrame'а в новый CSV файл
    merged_df.to_csv(output_file, index=False)


def main():
    # token_file_path = "authorization_code.txt"  # Путь к файлу с токеном
    # authorization_code = get_str_from_file(token_file_path)
    # get_token(authorization_code)
    # print(access_token)

    # raw_list = []
    # for i in range(100, 1000):
    #     print(i)
    #     r = get_data_from_api(f"https://shikimori.one/api/animes?page={i}&limit=50&kind=tv&",
    #                           access_token)
    #     if len(r) == 0:
    #         break
    #     for el in r:
    #         raw_list.append(el)

    # save_to_csv(raw_list, "raw_list2.csv")

    # merge_csv(["raw_list.csv", "raw_list2.csv"], "all_raw_list.csv")

    # all_list = []
    # df = pd.read_csv("all_raw_list.csv")
    # save_to_csv(all_list, "anime.csv")
    #
    # df = pd.read_csv("all_raw_list.csv")
    # with tqdm(total=len(df)) as pbar:
    #     for index, row in df.iterrows():
    #         r = get_data_from_api(f"https://shikimori.one/api{row['url']}", access_token, index)
    #         pbar.update(1)

    # df = pd.DataFrame(columns=["id", "name", "russian", "image", "url", "kind", "score", "status",
    #                            "episodes", "episodes_aired", "aired_on", "released_on", "rating",
    #                            "english", "japanese", "synonyms", "license_name_ru", "duration",
    #                            "description", "description_html", "description_source", "franchise",
    #                            "favoured", "anons", "ongoing", "thread_id", "topic_id", "myanimelist_id",
    #                            "rates_scores_stats", "rates_statuses_stats", "updated_at", "next_episode_at",
    #                            "fansubbers", "fandubbers", "licensors", "genres", "studios", "videos",
    #                            "screenshots", "user_rate"])

    data_list = []
    # Путь к директории с JSON файлами
    json_dir = 'jsons'

    # Проходим по всем файлам в директории
    for i in tqdm(range(len(os.listdir(json_dir)))):
        # Полный путь к файлу
        filepath = os.path.join(json_dir, f"get_request_{i}.json")

        # Открываем файл и загружаем его содержимое как словарь
        with open(filepath, 'r', encoding="utf-8") as file:
            json_data = json.load(file)
        data_list.append(json_data)
        # Добавляем словарь в DataFrame
        # df = df._append(json_data, ignore_index=True)
    df = pd.DataFrame.from_records(data_list)
    print(df.shape)
    df.to_csv('anime.csv', index=False, encoding="utf-8")


def normalize(d):
    nice_d: list[dict] = json.loads(d.replace("'", '"'))
    return [el['russian'] for el in nice_d]


def download_pictures(df):
    df['local_storage'] = None
    with tqdm(total=len(df)) as pbar:
        for index, row in df.iterrows():
            # d = json.loads(row['image'].replace("'", '"'))
            # get_data_from_api(f"https://shikimori.one{d['original']}",
            #                   access_token, index)
            df.at[index, 'local_storage'] = f"pics/get_request_{index}.jpg"
            pbar.update(1)
    df.to_csv("image.csv", encoding="utf-8")


if __name__ == "__main__":
    # main()
    # Create df for this task
    df = pd.read_csv("anime.csv")
    text_data = {
        'name': df['russian'],
        'score': df['score'],
        'image': df['image'],
        'aired_on': df['aired_on'],
        'description': df['description'],
        'genres': df['genres'].apply(normalize)
    }
    text_df = pd.DataFrame(text_data)
    text_df.dropna(inplace=True, subset=['name', 'description', 'genres', 'image'])
    print(len(text_df))
    # text_df.to_csv("text.csv", encoding="utf-8", index=False)
    # Download pictures

    download_pictures(text_df)
