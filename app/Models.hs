import Database.SQLite.Simple

main :: IO ()
main = do
    conn <- open "stock_db.db"

    -- Create tables
    execute_ conn "CREATE TABLE IF NOT EXISTS stock (\
                    \code TEXT PRIMARY KEY,\
                    \name TEXT\
                \)"
    execute_ conn "CREATE TABLE IF NOT EXISTS transactions (\
                    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
                    \stock TEXT,\
                    \type TEXT,\
                    \price REAL,\
                    \tax REAL,\
                    \FOREIGN KEY(stock) REFERENCES stock(code)\
                \)"
