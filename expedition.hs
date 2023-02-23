-- CPSC 312 - 2023 - Twenty Questions in Haskell
--  Copyright David Poole 2023, released on GNU General Public License

module TwentyQs where

import System.IO

data QATree = QLeaf String
            | QNode String QATree QATree
       deriving (Show)

initQATree = QNode "Is it living?"
                (QNode "Is it a person?"
                    (QLeaf "Justin Bieber")
                    (QLeaf "J-35, one of the southern resident killer whales"))
                (QNode "Is it a physical object?"
                    (QLeaf "Vancouver")
                    (QLeaf "CPSC 312"))

play :: QATree -> IO QATree
play tree =
   do
      putStrLn "Do you want to play 20 questions?"
      ans <- getLineFixed
      if (ans `elem` ["y","yes","ye","oui"])
        then do
           putStrLn "Think of an entity"
           newtree <- askabout tree
           play newtree
        else return tree

askabout :: QATree -> IO QATree
askabout (QLeaf ans) =
  do
    putStrLn("Is it "++ans++"?")
    line <- getLineFixed
    if (line `elem` ["y","yes","ye","oui"])
       then return (QLeaf ans)
       else do
          putStrLn("What were you thinking of?")
          obj <- getLineFixed
          putStrLn("Give a question for which the answer is yes for "++obj++" and no for "++ans)
          quest <- getLineFixed
          return (QNode quest (QLeaf obj) (QLeaf ans))
          
askabout (QNode q yes no) =
  do
    putStrLn(q)
    line <- getLineFixed
    if (line `elem` ["y","yes","ye","oui"])
       then do
            newyes <- (askabout yes)
            return (QNode q newyes no)
       else do
            newno <- askabout no
            return (QNode q yes newno)

getLineFixed =
   do
     line <- getLine
     return (fixdel2 line)
     
go :: IO QATree
go = play initQATree

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
