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
                            choice1 :: StoryPath,
                            choice2 :: StoryPath,
                            choice3 :: StoryPath}

-- Get node past a particular path
nodePastPath :: StoryTree -> String -> StoryTree
nodePastPath tree "1" = pathnode (choice1 tree)
nodePastPath tree "2" = pathnode (choice2 tree)
nodePastPath tree "3" = pathnode (choice3 tree)
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
         displayoutcome tree line
         play (movedown resources tree line)
      else play (resources, tree)


-- We move down to the selected option and modify the resources
movedown :: Resources -> StoryTree -> String -> (Resources, StoryTree)
movedown resources tree "1" = (traversechange resources (choice1 tree), nodePastPath tree "1")
movedown resources tree "2" = (traversechange resources (choice2 tree), nodePastPath tree "2")
movedown resources tree "3" = (traversechange resources (choice3 tree), nodePastPath tree "3")
--------------------------------------------------------------------------------------



-- Displaying to terminal ------------------------------------------------------------

-- TODO Rewrite this
-- Print the choices at the passed position
displaytreeoptions :: Resources -> StoryTree -> IO ()
displaytreeoptions resources tree = do
    displayoptionshelper resources tree (choice1 tree) "1"
    displayoptionshelper resources tree (choice2 tree) "2"
    displayoptionshelper resources tree (choice3 tree) "3"


-- Helper for displaytreeoptions
-- I tried using when but it was being weird
displayoptionshelper :: Resources -> StoryTree -> StoryPath -> String -> IO ()
displayoptionshelper resources tree path choicenum = if 
    (choicenum `elem` (checkAvailableOptions resources tree)) then
        putStrLn(option path) 
    else 
        pure () 

-- Display Resources (This should be implemented by deriving show, do change that)
displayResources :: Resources -> IO ()
displayResources (Resources health time icepick) = do
    putStrLn("Health: " ++ show health ++ "/10, Time: " ++ show time ++ " hours, Icepick:" ++ show icepick)

-- Displays the outcome of the chosen node
displayoutcome :: StoryTree -> String -> IO ()
displayoutcome tree "1" = do
    putStrLn("")
    putStrLn(result (choice1 tree))

displayoutcome tree "2" = do 
    putStrLn("")
    putStrLn(result (choice1 tree))

displayoutcome tree "3" = do 
    putStrLn("")
    putStrLn(result (choice1 tree))
--------------------------------------------------------------------------------------

     

-- Checking and Changing Resources ---------------------------------------------------

-- Modify a resource value based on an input number and a value increase or decrease
traversechange :: Resources -> StoryPath -> Resources
traversechange resources path = changeResource resources (getChange path)

-- Get an array of the available nodes 
checkAvailableOptions :: Resources -> StoryTree -> [String]
checkAvailableOptions resources tree = 
    [checkRequirements resources (choice1 tree) "1",
     checkRequirements resources (choice2 tree) "2",
     checkRequirements resources (choice3 tree) "3"]


-- Check if the requirements of an individual node are met 
-- if they are we return the option number otherwise we
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
------------------------------------------------------------------------------------------


-- Nodes for Prototype: Setting Up Story ------------------------------------
startingresources = Resources 10 8 1

endnode = StoryLeaf "The end."

dummypath = StoryPath endnode 3 5 0 0

firstchoice1 = StoryNode "You are at place 1" (StoryPath endnode 0 0 0 0 "Any. This is an ending..." "...") (StoryPath endnode 0 0 0 0 "Any. This is an ending..." "...") (StoryPath endnode 1 10 0 0 "Any. This is an ending..." "...")

firstchoice2 = StoryNode "You are at place 2" (StoryPath endnode 0 0 0 0 "Any. This is an ending..." "...") (StoryPath endnode 0 0 0 0 "Any. This is an ending..." "...") (StoryPath endnode 0 0 0 0 "Any. This is an ending..." "...")

firstchoice3 = StoryNode "You are at place 3" (StoryPath endnode 0 0 0 0 "Any. This is an ending..." "...") (StoryPath endnode 0 0 0 0 "Any. This is an ending..." "...") (StoryPath endnode 0 0 0 0 "Any. This is an ending..." "...")

startnode = StoryNode "You are on everest... lets get the fuck down" (StoryPath firstchoice1 0 0 1 (-5) "1. Jump" "Jumping...") (StoryPath firstchoice2 0 0 0 0 "2. Climb" "Climbing...") (StoryPath firstchoice3 0 0 0 0 "3. Run" "Running...")



main = do
    play (startingresources, startnode)
-- End of Basic Nodes -----------------------------------------------------
