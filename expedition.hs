-- CPSC 312 - 2023 - Twenty Questions in Haskell
--  Copyright David Poole 2023, released on GNU General Public License

import System.IO

-- String Option, String Result
-- Int Required Resource, Int Required Resource Amount
-- Int Resource Changed, Int Change Amount
-- Story Tree options 1, 2, 3
data StoryTree = StoryLeaf String String
            | StoryNode String String Int Int Int Int StoryTree StoryTree StoryTree


-- Basic loop of the game as we traverse the tree
-- We print options, detect input, then traverse the tree 
play :: StoryTree -> IO StoryTree
play tree =
   do
      putStrLn "You have reached the top of Mount Everest, you now have 8 hours to leave the death zone."
      printtreeoptions tree
      line <- getLineFixed
      if (line `elem` ["1","2","3"])
        then do
           newtree <- movedown tree line
           play newtree
        else return tree

movedown :: StoryTree -> String -> StoryTree
movedown tree line =
    -- if line == "num" get that storyTree
    -- subtract the resource change
    -- return the new option
    return tree


printtreeoptions :: StoryTree -> Bool
printtreeoptions tree =
    -- Look at all 3 options,
    -- Then print the option if the requirements are met
    return true

          
-- Get player input
getLineFixed =
   do
     line <- getLine
     return (fixdel line)


-- ...
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r


-- ...
splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t

-- ...
readcsv filename =
  do
    file <- readFile filename
    return [splitsep (==',') line| line <- splitsep (=='\n') file]
