module Types where

import Data.Time.Clock (UTCTime)

-- Type aliases
type Maze        = [[Tile]]
type Pos         = (Int, Int)   -- (x, y)
type ColoredChar = String

type Name    = String
type Seconds = Double
type Row     = [String]

-- Algebraic data types
data Tile       = Wall | Exit | Player | Unknown | Empty
                deriving Eq

data Color      = Green | Yellow | White | Gray | Reset

data Direction  = UpDir | DownDir | LeftDir | RightDir

data Difficulty = Easy | Medium | Hard | Insane
                deriving (Show, Read, Eq)

data GameState = GameState
    { gsMaze      :: Maze
    , gsName      :: Name
    , gsDiff      :: Difficulty
    , gsStartTime :: UTCTime
    , gsPos       :: Pos
    }

data Score = Score
    { playerName :: Name
    , difficulty :: Difficulty
    , timeTaken  :: Seconds
    } deriving Show

-- Type class
class Movable a where
    move :: a -> Pos -> Pos

-- Instances
instance Movable Direction where
    move dir (y, x) = case dir of
        UpDir    -> (y - 1, x)
        DownDir  -> (y + 1, x)
        LeftDir  -> (y, x - 1)
        RightDir -> (y, x + 1)

instance Ord Difficulty where
    compare a b = compare (rank a) (rank b)
        where
            rank :: Difficulty -> Int
            rank Easy   = 4
            rank Medium = 3
            rank Hard   = 2
            rank Insane = 1

instance Eq Score where
    a == b =
        timeTaken a  == timeTaken b &&
        difficulty a == difficulty b

instance Ord Score where
    compare a b =
        compare (timeTaken a) (timeTaken b) <>
        compare (difficulty a) (difficulty b)
