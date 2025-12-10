module Models where

import Database.SQLite.Simple
import Models (Stock)

connectDB :: IO Connection
connectDB = open "stocks.db"

insertStock :: Connection -> Stock -> IO ()
insertStock conn stock =
    execute_ conn "INSERT INTO stock (code, name) VALUES (?, ?)" stock

main :: IO ()
main = do
    conn <- connectDB

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
    
    -- Insert stock
    insertStock conn (Stock "AAPL" "Apple")

    close conn
