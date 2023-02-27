import System.IO

-- Right now the main issue with the whole thing is duplication of code for options "1" "2" "3"
-- We could change the choices to be an [StoryPath], though then we need to successfully cast the line to an int
-- If we do travel frequently to the same node through different paths should result be situation? and add string result to storypath



-- StoryTree Data Type -------------------------------------------------------------------------------------

-- String Option, String Result
-- StoryTree options 1, 2, 3 in the form of StoryPaths

data StoryTree = StoryLeaf String String
               | StoryNode String String StoryPath StoryPath StoryPath

-- Getters --
-- Get the choice of this node
getoption :: StoryTree -> String
getoption (StoryLeaf option result) = option
getoption (StoryNode option result choice1 choice2 choice3) = option

-- Get the message at this node
getresult :: StoryTree -> String
getresult (StoryLeaf option result) = result
getresult (StoryNode option result choice1 choice2 choice3) = result

-- Get path 1 
getchoice1 :: StoryTree -> StoryPath
getchoice1 (StoryNode option result choice1 choice2 choice3) = choice1

-- Get path 2
getchoice2 :: StoryTree -> StoryPath
getchoice2 (StoryNode option result choice1 choice2 choice3) = choice2

-- Get path 3
getchoice3 :: StoryTree -> StoryPath
getchoice3 (StoryNode option result choice1 choice2 choice3) = choice3

-- Get node past a particular path
nodePastPath :: StoryTree -> String -> StoryTree
nodePastPath tree "1" = getPathNode (getchoice1 tree)
nodePastPath tree "2" = getPathNode (getchoice2 tree)
nodePastPath tree "3" = getPathNode (getchoice3 tree)
----------------------------------------------------------------------------------



-- StoryPath Data Type -----------------------------------------------------------

-- StoryTree Node the Path leads to
-- Int Required Resource, Int Required Amount, Int Resource Changed, Int Change Amount

data StoryPath = StoryPath StoryTree Int Int Int Int

-- Get the node past the path
getPathNode :: StoryPath -> StoryTree
getPathNode (StoryPath tree rr ra rc ca) = tree

-- Get required to traverse
getRequiredResource :: StoryPath -> (Int, Int)
getRequiredResource (StoryPath tree rr ra rc ca) = (rr, ra)

-- Get change caused by traversal
getResourceChange :: StoryPath -> (Int, Int)
getResourceChange (StoryPath tree rr ra rc ca) = (rc, ca)
----------------------------------------------------------------------------------


-- Resources Data Type -----------------------------------------------------------

-- Maybe we can make this slightly more elegant? This is basically a weird implementation of a fixed size [Int] at the moment
-- We could also change this to use strings instead of numbers for identification
-- Health, Time, Icepick

data Resources = Resources Int Int Int

-- Get current value of a requested resource
getResource :: Resources -> Int -> Int
getResource (Resources health time icepick) 1 = health
getResource (Resources health time icepick) 2 = time
getResource (Resources health time icepick) 3 = icepick

-- Add change to a particular resource
changeResource :: Resources -> (Int, Int) -> Resources
changeResource (Resources health time icepick) (1, change) = (Resources (health+change) time icepick)
changeResource (Resources health time icepick) (2, change) = (Resources health (time+change) icepick)
changeResource (Resources health time icepick) (3, change) = (Resources health time (icepick+change))

----------------------------------------------------------------------------------



-- Looping and Traversing --------------------------------------------------------

-- Basic loop of the game as we traverse the tree
-- We print options, detect input, then traverse the tree 
play :: (Resources, StoryTree) -> IO StoryTree
play (resources, tree) =
   do
      putStrLn("")
      displaytreemessage tree
      putStrLn("")
      displayResources resources
      displaytreeoptions tree
      line <- getLineFixed
      if (line `elem` ["1","2","3"]) -- We need to go back to check these are actually met.
        then do
           play (movedown resources tree line)
        else return tree
   -- TODO: End on leaves


-- We move down to the selected option and modify the resources
movedown :: Resources -> StoryTree -> String -> (Resources, StoryTree)
movedown resources tree "1" = (traversechange resources (getchoice1 tree), nodePastPath tree "1")
movedown resources tree "2" = (traversechange resources (getchoice2 tree), nodePastPath tree "2")
movedown resources tree "3" = (traversechange resources (getchoice3 tree), nodePastPath tree "3")
--------------------------------------------------------------------------------------



-- Displaying to terminal ------------------------------------------------------------

-- Print the result at the passed position
displaytreemessage:: StoryTree -> IO ()
displaytreemessage tree = do
    putStrLn(getresult tree)


-- Print the choices at the passed position
displaytreeoptions :: StoryTree -> IO ()
displaytreeoptions tree = do
    displaytreechoice(nodePastPath tree "1")
    displaytreechoice(nodePastPath tree "2")
    displaytreechoice(nodePastPath tree "3")


-- Display the choice of passed node
displaytreechoice :: StoryTree -> IO ()
displaytreechoice tree = do
    putStrLn(getoption tree)


-- Display Resources (This should be implemented by deriving show, do change that)
displayResources :: Resources -> IO ()
displayResources (Resources health time icepick) = do
    putStrLn("Health: " ++ show health ++ "/10, Time: " ++ show time ++ " hours, Icepick:" ++ show icepick)
--------------------------------------------------------------------------------------

     

-- Checking and Changing Resources ---------------------------------------------------

-- Modify a resource value based on an input number and a value increase or decrease
traversechange :: Resources -> StoryPath -> Resources
traversechange resources path = changeResource resources (getResourceChange path)

     

-- Reusable nodes for using resources -------------------------------------------------
-- Takes the option and result for the next storypath
-- useicepick :: String -> String -> StoryNode -> StoryPath
-- useicepick option result nextnode = StoryPath 



-- Get an array of the nodes 
--checkAvailableOptions :: Resources -> StoryTree -> [String]
--checkAvailableOptions = []



--checkRequirements :: Resources -> StoryTree -> String -> String
--    checkRequirements resources node "1" = 

-----------------------------------------------------------------------------------------


-- Getting Player Input -----------------------------------------------------------------

-- Get player input
getLineFixed =
   do
     line <- getLine
     return (fixdel line)


fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r
------------------------------------------------------------------------------------------


-- Basic Nodes for Traversal Test -----------------------------------------
startingresources = Resources 10 8 1


-- dummy nodes 
dummy = StoryNode "" "Running..." (StoryPath endnode 0 0 0 0) (StoryPath endnode 0 0 0 0) (StoryPath endnode 0 0 0 0)

-- firstchoice3 = StoryNode "" "Climbing..." (StoryPath endnode 0 0 0 0) (StoryPath endnode 0 0 0 0) (StoryPath endnode 0 0 0 0)

endnode = StoryLeaf "Any. This is an ending..." "The end."

-- actual story
startnode = StoryNode "" "Congratulations! You've made it to the summit of everest! But can you make it down in one piece?"  (StoryPath beginjourney 0 0 0 0) (StoryPath dummy 3 2 0 0) (StoryPath dummy 3 2 0 0)

beginjourney = StoryNode "1. Begin your descent" "Good luck" (StoryPath journeybegunnode 0 0 0 0) (StoryPath dummy 0 0 0 0) (StoryPath dummy 0 0 0 0)

journeybegunnode = StoryNode "You begin making your way down from the summit, and you soon encounter a steep descent, but it's still flat enough to walk. Do you use your ice pick? (1 - yes) (2 - no)" "Ok, let's go..."  (StoryPath useicepick1 3 1 3 (-1)) (StoryPath dontuseicepick1 0 0 0 0) (StoryPath dummy 3 2 0 0)

useicepick1 = StoryNode "1. Use your ice pick" "Your ice pick gave you an easier time on the way down, but you cracked the handle and it's no longer usable. Was it worth it?" (StoryPath endnode 0 0 0 0) (StoryPath endnode 0 0 0 0) (StoryPath endnode 0 0 0 0)
dontuseicepick1 = StoryNode "2. Don't use it" "You stumbled a bit on the way down, but you managed to make it. Maybe the ice pick will be useful later..." (StoryPath endnode 0 0 0 0) (StoryPath endnode 0 0 0 0) (StoryPath endnode 0 0 0 0)



main = do
   play (startingresources, startnode)

-- End of Basic Nodes -----------------------------------------------------

