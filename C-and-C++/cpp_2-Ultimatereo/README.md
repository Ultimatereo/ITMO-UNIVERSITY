# Напоминание

## [NEW] Частичное тестирование
При каждом Pull Request будет запускаться первичная проверка сборки вашего решения, а также проверка на соответсвие clang format. Проверки неблокирующие и всё, что они могут сделать, это показать вам зелёную галочку или красный крестик.
Проверка clang format всегда будет отмечена галочкой. Посмотреть лог этапа можно в разделе Actions Github.
Подробнее можно почитать в конце документа с ТЗ.

## 0. С давно сделал работу, но забыл(а) до дедлайна сделать PR или поставить Reviewer. Можно проверить мою работу???
Нет. В таком случает считается, что вы не отправили работу до дедлайна на проверку и пропустили дедлайн.

## 1. Ветки
Всю разработку нужно вести на ветке **dev**, которую вы создаёте самостоятельно (изначально будет только main). При желании вы можете делать и другие ветки.

## 2. Pull Request
Для того, что проверяющий увидел отправку работы, важно не забыть выставить его ([@RonoveRaum](https://github.com/RonoveRaum)) в _Reviewers_.

## 3. Gitignore
В репозитерии не должны лежать .exe, idea/, .vs и прочие результаты сборки программы - только исходные файлы (.c(pp), .h(pp)).
Для этого настоятельно рекомендуется настроить .gitignore (или заглянуть в [репо](https://github.com/github/gitignore) и найти свой вариант).

## 4. Code style
Код, отправляемый на проверку, должен быть отформатирован согласно code style, заданному _.clang-format_ файлом, лежащим в репо.
Если вас не устраевает данный code style, то ничто вам не мешает форматировать код сразу перед отправкой на проверку, а при работе использовать своё форматирование.

Как применить .clang-format для разных IDE указано в [инструкции](https://docs.google.com/document/d/1rnDXh2Jxidppf81joDyM8XzNMINjpUYIfueFHSVCw9o/edit#heading=h.jauj3stwsulp). 

## 5. Коды возврата
По условиям работ вам необходимо завершаться с определёнными кодами возрата. Для удобства, эти коды вынесены в [return_codes.h](return_codes.h). Вам нужно лишь подключить этот загоовочный в свой код и использоватьмакросы из него.