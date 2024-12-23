import asyncio
import logging
import sys

from aiogram import Bot, Dispatcher, types
from aiogram.enums import ParseMode
from aiogram.filters import CommandStart, Command
from aiogram.types import Message

from movie_db import MovieDB
from movie_finder import MovieFinder

# Bot token can be obtained via https://t.me/BotFather
# BOT_TOKEN = getenv("BOT_TOKEN")
# VK_TOKEN = getenv("VK_TOKEN")
# KP_TOKEN = getenv("KP_TOKEN")
BOT_TOKEN = "6841969603:AAH-QgfwhRPfXZ5_sMhUMnBKMHvqd3T-QwY"
VK_TOKEN = ("vk1.a.-EtWed1xPcYWukhMGS3Ar4GDH2NC1TOi2kiWPtAXN9UIeimP7pZoD9Sv9KxqeOL9i99KzD3gmllK_dgbX7UluC"
            "-CgljGuYN7UzRXHl0AsUhEvMZxMJeEE8fVWYQsFLsiXsp2_0eD-ZoVm_RdT"
            "-nJWFMnDnwzd3DW4FpfJcWdG0x6nmkydbNd97bc64a7ervxe0PebalyN_nVJBvZbz011g")
KP_TOKEN = "FR59T25-AZC4GX7-GTKV9AT-377AA3A"
# All handlers should be attached to the Router (or Dispatcher)
dp = Dispatcher()
movie_finder = MovieFinder(VK_TOKEN, KP_TOKEN)
# DataBase initialized
db = MovieDB()
db.connect()
mem_film = db.load_mem_film()


@dp.message(CommandStart())
async def command_start_handler(message: Message) -> None:
    """
    This handler receives messages with `/start` command
    """
    # Most event objects have aliases for API methods that can be called in events' context
    # For example if you want to answer to incoming message you can use `message.answer(...)` alias
    # and the target chat will be passed to :ref:`aiogram.methods.send_message.SendMessage`
    # method automatically or call API method directly via
    # Bot instance: `bot.send_message(chat_id=message.chat.id, ...)`
    await message.answer(f"Привет, *{message.from_user.full_name}*!\n"
                         f"Этот бот поможет тебе найти фильм, который ты хочешь посмотреть!\n"
                         f"Просто напиши название фильма и вперёд!")


@dp.message(Command("history"))
async def command_history_handler(message: Message) -> None:
    """
    This handler receives messages with `/history` command
    It should return the history of last queries of the user.
    """
    hist = db.get_n_last_requests(message.from_user.id)
    text = f"Вот ваши последние {len(hist)} запросов:\n" + "\n".join([h[1] for h in hist])
    await message.answer(text)


@dp.message(Command("stats"))
async def command_stats_handler(message: Message) -> None:
    """
    This handler receives messages with `/stats` command
    It should return the amount of times that every film was recommended to the user .
    """
    stat = db.get_movie_statistics(message.from_user.id)
    text = f"Вот ваши топ {len(stat)} фильмов:\n" + "\n".join([' '.join(map(str, s)) for s in stat])
    await message.answer(text)


@dp.message(Command("help"))
async def command_help_handler(message: Message) -> None:
    """
    This handler receives messages with `/help` command
    It should return the manual for the user.
    """
    text = '''Как работает бот?

- Информация о фильме ботом ищется на кинопоиске, а сам фильм на вк, и потом весь результат объединяется в сообщение.
- Реализовано простое кеширование результата для быстрого доступа к запросам, которые уже были

Функционал Бота

- По дефолту бот возвращает нужный фильм по запросу
- /history - список последних 20 запросов пользователей
- /stats - статистика по часто полученным фильмам пользователем
- /help - помощь по боту'''
    await message.answer(text)


@dp.message()
async def film_handler(message: types.Message) -> None:
    """
    Handler will send info about the film the user asked.
    """
    try:
        if message.text:
            # Send a film corresponding to the text
            query = ' '.join(word for word in message.text.split() if len(word) > 2).lower()
            if query not in mem_film:
                mem_film[query] = movie_finder.search(query)
            film_poster_url, film_text, video_desc, film_name = mem_film[query]
            await message.answer_photo(photo=film_poster_url, caption=video_desc, parse_mode=ParseMode.MARKDOWN)
            await message.answer(film_text, parse_mode=ParseMode.MARKDOWN)
            db.add_movie_request(message.from_user.id, message.text, film_name, film_poster_url, film_text, video_desc)
        else:
            # Anything than text should be ignored
            await message.answer("Название фильма надо написать текстом, гифки, видео и любой "
                                 "другой формат помимо текста не принимается ботом(")
    except:
        await message.answer("Произошла ошибка. Вы уверены, что правильно выполнили запрос?")


async def main() -> None:
    # Initialize Bot instance with a default parse mode which will be passed to all API calls
    bot = Bot(BOT_TOKEN, parse_mode=ParseMode.MARKDOWN)
    # And the run events dispatching
    await dp.start_polling(bot)


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, stream=sys.stdout)
    asyncio.run(main())
