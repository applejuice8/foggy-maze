data Stock = Stock
    { stockCode :: String,
    , stockName :: String
    } deriving Show

data BuySell = Buy | Sell
    deriving (Show, Eq)

data Transaction = Transaction
    { stockCode :: String
    , type :: BuySell
    , price :: Double
    , tax :: Double
    } deriving Show
