import System.IO

-- String Option, String Result
-- Int Required Resource, Int Required Resource Amount
-- Int Resource Changed, Int Change Amount
-- Story Tree options 1, 2, 3
data StoryTree = StoryLeaf {option :: String,
                            result :: String}
               | StoryNode {option :: String,
                            result :: String,
                            reqresource :: Int,
                            reqamount :: Int,
                            changeresource :: Int,
                            changeamount :: Int,
                            option1 :: StoryTree,
                            option2 :: StoryTree,
                            option3 :: StoryTree}


-- Basic loop of the game as we traverse the tree
-- We print options, detect input, then traverse the tree 
play :: StoryTree -> StoryTree
play tree =
   do
      printtreemessage tree
      printtreeoptions tree
      line <- getLineFixed
      if (line `elem` ["1","2","3"]) -- We need to go back to check these are actually met.
        then do
           newtree <- movedown tree line
           play newtree
        else return tree


-- We move down to the selected option and
movedown :: StoryTree -> String -> StoryTree
movedown tree line =
    -- if line == "num" get that storyTree
    -- subtract the resource change
    -- return the new option
    return tree

printtreemessage:: StoryTree -> ()
printtreemessage tree = do
    putStrLn(result tree)


printtreeoptions :: StoryTree -> ()
printtreeoptions tree = do
    printtreechoice(option1)
    printtreechoice(option2)
    printtreechoice(option3)


printtreechoice :: StoryTree -> ()
printtreechoice tree = do
    putStrLn(option tree)
          
-- Get player input
getLineFixed =
   do
     line <- getLine
     return (fixdel line)


-- Basic Nodes for Traversal Test (We should read from csv if there's time)




main = do
    print("THIS COMPILES")

-- End of Basic Nodes -----------------------------------------------------


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
