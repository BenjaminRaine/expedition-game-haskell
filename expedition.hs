-- CPSC 312 - 2023 - Twenty Questions in Haskell
--  Copyright David Poole 2023, released on GNU General Public License

module EverestEscape where

import System.IO

import HashTreeDict

-- Current location in the story, Health, Time, Ice pick
-- also used at certain branches to check for the required state
data State = State Path Int Double Bool    -- currentBranch, health, timeLeft, icePick
               deriving (Show)

-- planning for tree, have each branch be its own event, and include a "required state" to be at that branch.
-- if the player doesn't have the required state, sent to a bad ending

data Path = PathEnding String
            | PathBranch String Path Path
       deriving (Show)

-- do we even need this
-- current state in the game, is the key, possible future paths is the value???
-- type Mem = Dict State [Path]

initPath = PathBranch "Is it living?"
                (PathBranch "Is it a person?"
                    (PathEnding "Justin Bieber")
                    (PathEnding "J-35, one of the southern resident killer whales"))
                (PathBranch "Is it a physical object?"
                    (PathEnding "Vancouver")
                    (PathEnding "CPSC 312"))

play :: Path -> State -> IO State
play tree state =
   let (currentBranch, health, timeLeft, icePick) = state in
   do
      putStrLn "Congratulations! You've reached the summit of Mount Everest! You only have " ++ show timeLeft ++ "hours, "++show health" health points, and an ice pick to get you back down. Can you do it?"
      ans <- getLineFixed
      if (ans `elem` ["y","yes","ye","oui"])
        then do
           putStrLn "Think of an entity"
           newtree <- askabout tree
           play newtree
        else return tree

askabout :: Path -> IO Path
askabout (PathEnding ans) =
  do
    putStrLn("Is it "++ans++"?")
    line <- getLineFixed
    if (line `elem` ["y","yes","ye","oui"])
       then return (PathEnding ans)
       else do
          putStrLn("What were you thinking of?")
          obj <- getLineFixed
          putStrLn("Give a question for which the answer is yes for "++obj++" and no for "++ans)
          quest <- getLineFixed
          return (PathBranch quest (PathEnding obj) (PathEnding ans))
          
askabout (PathBranch q yes no) =
  do
    putStrLn(q)
    line <- getLineFixed
    if (line `elem` ["y","yes","ye","oui"])
       then do
            newyes <- (askabout yes)
            return (PathBranch q newyes no)
       else do
            newno <- askabout no
            return (PathBranch q yes newno)

getLineFixed =
   do
     line <- getLine
     return (fixdel2 line)
     
go :: IO State
go = play initPath (State initPath 10 8.0 True)

----- Two Implementations fo fixdel ----

-- fixdel removes deleted elements from string
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r

-- fixdel2 deleted elements from string 
fixdel2 :: [Char] -> [Char]
fixdel2 st = fst (remdel2 st)
-- remdel2 st   returns (resulting_string, number_of_deletes_to_do)
remdel2 :: [Char] -> ([Char], Int)
remdel2 [] = ([],0)
remdel2 ('\DEL':t) = (s,n+1)
    where (s,n) = remdel2 t
remdel2 (h:t)
    | n>0 = (s,n-1)
    | otherwise = (h:s,0)
    where (s,n) = remdel2 t

splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t

readcsv filename =
  do
    file <- readFile filename
    return [splitsep (==',') line| line <- splitsep (=='\n') file]
