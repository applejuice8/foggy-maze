type Name = String
type Seconds = Double

data Score = Score
    { playerName :: Name
    , timeTaken :: Seconds
    } deriving (Show, Ord, Eq)

scoreToRecord :: Score -> [String]
scoreToRecord (Score name timeTaken) =
    [name, show timeTaken]

recordToScore :: [String] -> Either String Score
recordToScore [name, timeTaken] =
    Right (Score name (read timeTaken))
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
