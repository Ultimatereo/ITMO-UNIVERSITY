[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/qDJFQXSE)
# lab-template

Шаблонный репозиторий лабораторный по курсу Машинного Обучения

Используйте ссылку на задания в Github Classroom, чтобы преподаватель мог проверить ваш код

# Как работает код?

## Весь код располагается в [parser.py](https://github.com/itmo-ml-lab-ML-course/parser-Ultimatereo/blob/main/cian_parser/parser.py)
Комментарии о том, что делают различные функции, расписаны собственно в коде.
## Для корректной работы кода надо подготовить два файла формата .har.
- Один из них это [request_page.har](https://github.com/itmo-ml-lab-ML-course/parser-Ultimatereo/blob/main/cian_parser/assets/request_page.har), в нём должен быть формат request, который прогружает страничку на Циане подобной [этой](https://spb.cian.ru/cat.php?deal_type=sale&engine_version=2&object_type%5B0%5D=2&offer_type=flat&p=3&region=2). 
- А второй это [request_flat.har](https://github.com/itmo-ml-lab-ML-course/parser-Ultimatereo/blob/main/cian_parser/assets/request_flat.har), в нём должен быть формат request, который прогружает уже страничку конкретной квартиры подобной [этой](https://spb.cian.ru/sale/flat/293601226/).

