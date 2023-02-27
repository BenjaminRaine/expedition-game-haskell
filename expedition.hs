import System.IO

-- Things to improve -----------------------------------------------------------------------------------------------------------------
-- Right now the main issue with the whole thing is duplication of code for options "1" "2" "3"
-- We could change the choices to be an [StoryPath], though then we need to successfully cast the line to an int
-- Another future TODO would be to make it so that we can pass changes in multiple elements ie time and health
---------------------------------------------------------------------------------------------------------------------------------------


-- StoryTree Data Type -------------------------------------------------------------------------------------

-- String Option, String Result
-- StoryTree options 1, 2, 3 in the form of StoryPaths

data StoryTree = StoryLeaf {situation :: String}

               | StoryNode {situation :: String,
                            choices :: [StoryPath]}

-- Get node past a particular path
nodePastPath :: StoryTree -> Int -> StoryTree
nodePastPath tree i = pathnode ((choices tree) !! i)
----------------------------------------------------------------------------------



-- StoryPath Data Type -----------------------------------------------------------

-- StoryTree Node the Path leads to
-- Int Required Resource, Int Required Amount, Int Resource Changed, Int Change Amount

data StoryPath = StoryPath {pathnode :: StoryTree,
                            reqtype :: Int,
                            reqamt :: Int,
                            changetype :: Int,
                            changeamt :: Int,
                            option :: String,
                            result :: String}

-- Get required to traverse
getRequired :: StoryPath -> (Int, Int)
getRequired (StoryPath _ rr ra _ _ _ _) = (rr, ra)

-- Get change caused by traversal
getChange :: StoryPath -> (Int, Int)
getChange (StoryPath _ _ _ rc ca _ _) = (rc, ca)

----------------------------------------------------------------------------------


-- Resources Data Type -----------------------------------------------------------

-- Maybe we can make this slightly more elegant? This is basically a weird implementation of a fixed size [Int] at the moment
-- We could also change this to use strings instead of numbers for identification
-- Health, Time, Icepick

data Resources = Resources {health :: Int,
                            time :: Int,
                            icepick :: Int}

-- Get current value of a requested resource
getResource :: Resources -> Int -> Int
getResource _ 0 = 0
getResource (Resources h _ _) 1 = h
getResource (Resources _ t _) 2 = t
getResource (Resources _ _ p) 3 = p

-- Add change to a particular resource
changeResource :: Resources -> (Int, Int) -> Resources
changeResource current (0, _) = current
changeResource (Resources h t p) (1, change) = Resources (h+change) t p
changeResource (Resources h t p) (2, change) = Resources h (t+change) p
changeResource (Resources h t p) (3, change) = Resources h t (p+change)

----------------------------------------------------------------------------------



-- Looping and Traversing --------------------------------------------------------

-- Basic loop of the game as we traverse the tree
-- We print options, detect input, then traverse the tree 
play :: (Resources, StoryTree) -> IO ()
play (resources, (StoryLeaf s)) = do
     putStrLn("")
     putStrLn(s)

play (resources, tree) = do
    putStrLn("")
    putStrLn(situation tree)
    putStrLn("")
    displayResources resources
    displaytreeoptions resources tree
    line <- getLineFixed
    if (line `elem` (checkAvailableOptions resources tree)) -- We need to go back to check these are actually met.
      then do
         play (movedown resources tree line)
      else play (resources, tree)


-- We move down to the selected option and modify the resources
movedown :: Resources -> StoryTree -> String -> (Resources, StoryTree)
movedown resources tree i = (traversechange resources ((choices tree) !! (strchoicetoindex i)), nodePastPath ((choices tree) !! strchoicetoindex(i)))
--------------------------------------------------------------------------------------



-- Displaying to terminal ------------------------------------------------------------

-- Print the choices at the passed position
displaytreeoptions :: Resources -> StoryTree -> IO ()
displaytreeoptions resources tree = map [putStrLn(option o) | o <- (choices tree), o `elem` (checkAvailableOptions resources tree)]

-- Display Resources (This should be implemented by deriving show, do change that)
displayResources :: Resources -> IO ()
displayResources (Resources health time icepick) = do
    putStrLn("Health: " ++ show health ++ "/10, Time: " ++ show time ++ " hours, Icepick:" ++ show icepick)
--------------------------------------------------------------------------------------

     

-- Checking and Changing Resources ---------------------------------------------------

-- Modify a resource value based on an input number and a value increase or decrease
traversechange :: Resources -> StoryPath -> Resources
traversechange resources path = changeResource resources (getChange path)

-- Get an array of the available nodes 
checkAvailableOptions :: Resources -> StoryTree -> [String]
checkAvailableOptions resources tree = checkAvailableReccursive resources (choices tree) (length (choices tree))

-- Recursive Helper for checkAvailableOptions
-- I know this function is a bit messy I might rewrite this
checkAvailableReccursive :: Resources -> [StoryPath] -> Int -> [String]
checkAvailableReccursive resources clist 0 = []
checkAvailableReccursive resources clist i = (checkRequirements resources (clist !! i-1) (show i)) : checkAvailableReccursive resources clist i

-- Check if the requirements of an individual node are met 
-- if they are we return the option number otherwise we return ""
checkRequirements :: Resources -> StoryPath -> String -> String
checkRequirements resources path choicenum = let
    (rr, ra) = getRequired path
    ca = getResource resources rr
    shoulddis = ca >= ra
    in if shoulddis then choicenum else ""
-----------------------------------------------------------------------------------------


-- Getting Player Input -----------------------------------------------------------------

-- Get player input
getLineFixed = do
    line <- getLine
    return (fixdel line)


fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r


strchoicetoindex :: String -> Int
strchoicetoindex line = read line - 1

------------------------------------------------------------------------------------------


-- Nodes for Prototype: Setting Up Story ------------------------------------
startingresources = Resources 10 8 1

endnode = StoryLeaf "The end."

firstchoice1 = StoryNode "You are at place 1" [(StoryPath endnode 0 0 0 0 "1. This is an ending..." "..."), (StoryPath endnode 0 0 0 0 "2. This is an ending..."  "..."), (StoryPath endnode 1 10 0 0 "3. This is an ending..." "...")]

firstchoice2 = StoryNode "You are at place 2" [(StoryPath endnode 0 0 0 0 "1. This is an ending..." "..."), (StoryPath endnode 0 0 0 0 "2. This is an ending..."  "..."), (StoryPath endnode 0 0 0 0 "3. This is an ending..." "...")]

firstchoice3 = StoryNode "You are at place 3" [(StoryPath endnode 0 0 0 0 "1. This is an ending..." "...")]

startnode = StoryNode "You are on everest... lets get the fuck down" [(StoryPath firstchoice1 0 0 1 (-5) "1. Jump" "Jumping..."), (StoryPath firstchoice2 0 0 0 0 "2. Climb" "Climbing..."), (StoryPath firstchoice3 0 0 0 0 "3. Run" "Running...")]


main = do
    play (startingresources, startnode)
-- End of Basic Nodes -----------------------------------------------------
