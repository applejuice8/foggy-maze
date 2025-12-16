import System.Directory (doesFileExist)
import Text.CSV (parseCSVFromFile, printCSV)
import Data.List (sortOn)

type Name = String
type Seconds = Double

data Score = Score
    { playerName :: Name
    , timeTaken  :: Seconds
    } deriving (Show, Eq, Ord)

scoreToRecord :: Score -> [String]
scoreToRecord (Score playerName timeTaken) =
    [playerName, show timeTaken]

recordToScore :: [String] -> Either String Score
recordToScore [playerName, timeTaken] =
    Right (Score playerName (read timeTaken))
recordToScore _ =
    Left "Invalid CSV record"

readCSV :: FilePath -> IO [[String]]
readCSV file = do
    exists <- doesFileExist file
    if not exists
        then return []
        else do
            result <- parseCSVFromFile file
            case result of
                Right rows -> return rows
                Left _     -> return []

writeScore :: FilePath -> Score -> IO ()
writeScore file score = do
    rows <- readCSV file
    let newRows = rows ++ [scoreToRecord score]
    writeFile file (printCSV newRows)

readScores :: FilePath -> IO [Score]
readScores file = do
    rows <- readCSV file
    return
        [ score
        | Right score <- map recordToScore rows
        ]

topScores :: Int -> [Score] -> [Score]
topScores n =
    take n . sortOn timeTaken

main :: IO ()
main = do
    let file = "app/scores.csv"

    writeScore file (Score "Colin" 12.38)
    writeScore file (Score "Alice" 9.12)

    scores <- readScores file
    mapM_ print (topScores 5 scores)
