import sqlite3
from datetime import datetime


class MovieDB:
    def __init__(self):
        # Create a connection to the database
        self.conn = None
        self.cursor = None

    def connect(self):
        self.conn = sqlite3.connect('movie_database.db')
        self.cursor = self.conn.cursor()

        # Create a table
        self.cursor.execute('''
            CREATE TABLE IF NOT EXISTS MovieRequests (
                Id INTEGER PRIMARY KEY AUTOINCREMENT,
                Time DATETIME,
                ChatId INTEGER,
                Query TEXT,
                Name TEXT,
                FilmPosterURL TEXT,
                FilmText TEXT,
                VideoDesc TEXT
            )
        ''')
        self.conn.commit()

    def get_movie_info(self, query):
        self.cursor.execute(
            "SELECT FilmPosterURL, FilmText, VideoDesc, Name FROM MovieRequests WHERE Query=?", (query,))
        return self.cursor.fetchone()

    def add_movie_request(self, chat_id, query, name, film_poster_url, film_text, video_desc):
        """
        Add a movie request to the database
        """
        current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        self.cursor.execute('''
            INSERT INTO MovieRequests (Time, ChatId, Query, Name, FilmPosterURL, FilmText, VideoDesc)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        ''', (current_time, chat_id, query, name, film_poster_url, film_text, video_desc))
        self.conn.commit()

    def get_n_last_requests(self, chat_id, n=20):
        """
        Get n last requests for a specific user
        """
        self.cursor.execute(f'''
            SELECT Time, Query, Name
            FROM MovieRequests
            WHERE ChatId = ?
            ORDER BY Time DESC
            LIMIT {n}
        ''', (chat_id,))
        result = self.cursor.fetchall()
        return result

    def get_movie_statistics(self, chat_id, n=20):
        # Get movie statistics for a specific user
        self.cursor.execute(f'''
            SELECT Name, COUNT(*) as Count
            FROM MovieRequests
            WHERE ChatId = ?
            GROUP BY Name
            ORDER BY Count DESC
            LIMIT {n}
        ''', (chat_id,))
        result = self.cursor.fetchall()
        return result

    def close(self):
        self.conn.close()

    def load_mem_film(self):
        query_map = {}
        self.cursor.execute("SELECT Query, FilmPosterURL, FilmText, VideoDesc, Name FROM MovieRequests")
        rows = self.cursor.fetchall()
        for row in rows:
            query = row[0]
            query = ' '.join(word for word in query.split() if len(word) > 2).lower()
            info_tuple = (row[1], row[2], row[3], row[4])
            query_map[query.lower()] = info_tuple
        return query_map
