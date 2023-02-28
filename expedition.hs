import System.IO

-- StoryTree Data Type -------------------------------------------------------------------------------------

-- String Option, String Result
-- StoryTree options 1, 2, 3 in the form of StoryPaths

data StoryTree = StoryLeaf {situation :: String}

               | StoryNode {situation :: String,
                            choice1 :: StoryPath,
                            choice2 :: StoryPath}

-- Get node past a particular path
nodePastPath :: StoryTree -> String -> StoryTree
nodePastPath tree "1" = pathnode (choice1 tree)
nodePastPath tree "2" = pathnode (choice2 tree)
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

-- Empty Resource Ending Nodes
nohealthleaf = StoryLeaf("You've run out of health.")
notimeleaf = StoryLeaf("You've run out of oxygen.")
----------------------------------------------------------------------------------



-- Looping and Traversing --------------------------------------------------------

-- Basic loop of the game as we traverse the tree
-- We print options, detect input, then traverse the tree 
play :: (Resources, StoryTree) -> IO ()

play (resources, (StoryLeaf s)) = 
    if (health resources) <= 0 then
        play ((Resources 1 1 1), nohealthleaf)
    else if (time resources) <= 0 then
        play ((Resources 1 1 1), notimeleaf)
    else do
        putStrLn("")
        putStrLn(s)

play (resources, tree) =
    if (health resources) <= 0 then
        play ((Resources 1 1 1), nohealthleaf)
    else if (time resources) <= 0 then
        play ((Resources 1 1 1), notimeleaf)
    else do
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
--------------------------------------------------------------------------------------



-- Displaying to terminal ------------------------------------------------------------

-- Print the choices at the passed position
displaytreeoptions :: Resources -> StoryTree -> IO ()
displaytreeoptions resources tree = do
    displayoptionshelper resources tree (choice1 tree) "1"
    displayoptionshelper resources tree (choice2 tree) "2"


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
    putStrLn(result (choice2 tree))

--------------------------------------------------------------------------------------

     

-- Checking and Changing Resources ---------------------------------------------------

-- Modify a resource value based on an input number and a value increase or decrease
traversechange :: Resources -> StoryPath -> Resources
traversechange resources path = changeResource resources (getChange path)

-- Get an array of the available nodes 
checkAvailableOptions :: Resources -> StoryTree -> [String]
checkAvailableOptions resources tree = 
    fixfilter [checkRequirements resources (choice1 tree) "1",
               checkRequirements resources (choice2 tree) "2"]


-- Check if the requirements of an individual node are met 
-- if they are we return the option number otherwise we
checkRequirements :: Resources -> StoryPath -> String -> String
checkRequirements resources path choicenum = let
    (rr, ra) = getRequired path
    ca = getResource resources rr
    shoulddis = ca >= ra
    in if shoulddis then choicenum else ""
    
fixfilter st
   | "" `elem` st = fixfilter (removeemptystring st)
   | otherwise = st
removeemptystring ("":r) = r
removeemptystring (a:"":r) = a:r
removeemptystring (a:r) = a: removeemptystring r
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

-- health, time, ice pick
startingresources = Resources 10 8 1

endnode = StoryLeaf "Congratulations! You've made it back to base camp. It should be a smooth journey back home :)"

dummypath = (StoryPath endnode 3 5 0 0 "" "")

almostAtBaseCamp = StoryNode "You keep on going and you see the base camp in the distance. You're almost there! But the winds are starting to pick up again! Do you decide to power through the wind, or wait until it passes?" (StoryPath endnode 1 2 1 (-2) "1: Power through (-2 health)" "Eager to get back, you decide to power through") (StoryPath endnode 2 2 2 (-2) "2: Wait it out (-2 hours)" "You decide not to risk it and wait it out before heading down")

battleYetIcePick = StoryNode "You use your ice pick to fight the yeti off and win, but it breaks in the process. This is your territory now!" (StoryPath almostAtBaseCamp 0 0 0 0 "1: Continue" "") dummypath

battleYetBareHands = StoryNode "You win! But not without a price. The yeti gravely injures you and you lose 5 health points!" (StoryPath almostAtBaseCamp 0 0 0 0 "1: Continue" "") dummypath

yetiEncounter = StoryNode "After walking for an hour, you find the creature the made those footprints! The Yeti! And you have entered his territory!" (StoryPath battleYetIcePick 3 1 3 (-1) "1: Battle yeti with ice pick (-1 ice pick)" "You wield your ice pick like a soldier ready to defend his homeland") (StoryPath battleYetBareHands 1 5 1 (-5) "2: Fight with your bare hands" "You hold up your fists and hope for the best")

footPrintEncounter = StoryNode "While you continue your journey, you notice some very large, unhuman footprints. Do you continue to go in this direction and hope that you don't meet the creature that made them, or do you decide to spend an extra 3 hours taking the long way avoiding it?" (StoryPath yetiEncounter 2 1 2 (-1) "1. Continue in this direction (-1 hour)" "You decide to continue. Let's hope we don't find something bad.") (StoryPath  almostAtBaseCamp 2 3 2 (-3) "2: Take the long way around (-3 hours)" "You decide to go the long way around and avoid whatever there might have been.")

blizzardEncounter = StoryNode "You've been trudging along peacefully for some time but noticed that the winds have been picking up and the snow is falling hard. It's a blizzard! There's a cave nearby to seek shelter. Do you decide to wait out the storm in a cave, or do you keep going through it?" (StoryPath footPrintEncounter 1 5 1 (-5) "1. Continue through the blizzard (-5 health)" "You decide to continue through the blizzard in order to save time, but lost 5 health points because of the hypothermia. Was it worth it?") (StoryPath footPrintEncounter 2 2 2 (-2) "2. Wait it out in the cave (-2 hours)" "You decided to wait it out in the cave for two hours. After you emerge, the weather seems to have cleared. Will you still have enough time to get down?")

beginJourney = StoryNode "You begin making your way down from the summit, and you soon encounter a steep descent, but it's still flat enough to walk. Do you use your ice pick?" (StoryPath blizzardEncounter 3 1 3 (-1) "1. Use ice pick (-1 ice pick)" "Your ice pick gave you an easier time on the way down, but you cracked the handle and it's no longer usable. Was it worth it?") (StoryPath blizzardEncounter 2 1 2 (-1) "2. Don't use ice pick (-1 hour)" "You stumbled a bit on the way down and it took a bit longer, but you managed to get through. Maybe the ice pick will be useful later...")

startnode = StoryNode "Congratulations! You've made it to the top of Mount Everest! But can you get down in one piece?" (StoryPath beginJourney 0 0 0 0 "1. Begin your descent" "Good luck...") dummypath

main = do
    play (startingresources, startnode)
-- End of Basic Nodes -----------------------------------------------------
