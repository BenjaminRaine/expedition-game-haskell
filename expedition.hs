import System.IO

-- String Option, String Result
-- Int Required Resource, Int Required Resource Amount
-- Int Resource Changed, Int Change Amount
-- Story Tree options 1, 2, 3
data StoryTree = StoryLeaf {option :: String,
                            result :: String,
                            reqresource :: Int,
                            reqamount :: Int,
                            changeresource :: Int,
                            changeamount :: Int}
               | StoryNode {option :: String,
                            result :: String,
                            reqresource :: Int,
                            reqamount :: Int,
                            changeresource :: Int,
                            changeamount :: Int,
                            choice1 :: StoryTree,
                            choice2 :: StoryTree,
                            choice3 :: StoryTree}


-- Basic loop of the game as we traverse the tree
-- We print options, detect input, then traverse the tree 
play :: StoryTree -> IO StoryTree
play tree =
   do
      displaytreemessage tree
      displaytreeoptions tree
      line <- getLineFixed
      if (line `elem` ["1","2","3"]) -- We need to go back to check these are actually met.
        then do
           play (movedown tree line)
        else return tree


-- We move down to the selected option and modify the resources
movedown :: StoryTree -> String -> StoryTree
movedown tree "1" =
    do
        resourcechange (choice1 tree)
        return (choice1 tree)

movedown tree "2" =
    do
        resourcechange (choice1 tree)
        return (choice1 tree)

movedown tree "3" = 
    do
        resourcechange (choice1 tree)
        return (choice1 tree)


-- Print the result at the passed position
displaytreemessage:: StoryTree -> IO ()
displaytreemessage tree = do
    putStrLn(result tree)


-- Print the choices at the passed position
displaytreeoptions :: StoryTree -> IO ()
displaytreeoptions tree = do
    displaytreechoice(choice1 tree)
    displaytreechoice(choice2 tree)
    displaytreechoice(choice3 tree)


-- Display the choice of passed node
displaytreechoice :: StoryTree -> IO ()
displaytreechoice tree = do
    putStrLn(option tree)
     

-- Modify a resource value based on an input number and a value increase or decrease
resourcechange :: StoryTree -> ()
resourcechange = 1 -- need to implement resource management
     

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
