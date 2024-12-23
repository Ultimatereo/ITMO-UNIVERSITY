import os

import pandas as pd
from shutil import copyfile

from tqdm import tqdm


def prepare_data():
    # Загрузка данных
    anime = pd.read_csv('../text-Ultimatereo/image.csv')
    anime['decade'] = anime['aired_on'].apply(lambda date: str(date)[:3] + '0')
    anime.to_csv("anime.csv", encoding="utf-8", index=False)


def sort_pics():
    df = pd.read_csv('anime.csv')

    # Создание папки pics, если она не существует
    output_folder = 'pics'
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    output_train_folder = os.path.join(output_folder, "train")
    if not os.path.exists(output_train_folder):
        os.makedirs(output_train_folder)

    output_test_folder = os.path.join(output_folder, "test")
    if not os.path.exists(output_test_folder):
        os.makedirs(output_test_folder)

    # Проход по каждому десятилетию и копирование файлов в соответствующую папку
    for decade in df['decade'].unique():
        if decade == "1990" or decade[0] == "2":
            decade_train_folder = os.path.join(output_train_folder, str(decade))
            if not os.path.exists(decade_train_folder):
                os.makedirs(decade_train_folder)

            decade_test_folder = os.path.join(output_test_folder, str(decade))
            if not os.path.exists(decade_test_folder):
                os.makedirs(decade_test_folder)

            train_size = int(0.8 * df[df['decade'] == decade].shape[0])
            i = 0
            for index, row in tqdm(df[df['decade'] == decade].iterrows()):
                image_path = "../text-Ultimatereo/" + row['local_storage']
                if os.path.exists(image_path):
                    image_name = os.path.basename(image_path)
                    if i < train_size:
                        copyfile(image_path, os.path.join(decade_train_folder, image_name))
                    else:
                        copyfile(image_path, os.path.join(decade_test_folder, image_name))
                else:
                    print(f"Файл {image_path} не найден.")
                i += 1

    print("Готово.")


if __name__ == "__main__":
    # prepare_data()
    sort_pics()
