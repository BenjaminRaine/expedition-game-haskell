import System.IO

-- Right now the main issue with the whole thing is duplication of code for options "1" "2" "3"
-- If there's time we could go back to create a StoryPath object of (StoryTree Int Int Int Int)
-- Then change StoryNode to String String [StoryPath, StoryPath, StoryPath]



-- StoryTree Data Type -------------------------------------------------------------------------------------

-- String Option, String Result
-- Int Required Resource, Int Required Resource Amount
-- Int Resource Changed, Int Change Amount
-- Story Tree options 1, 2, 3

data StoryTree = StoryLeaf String String Int Int
               | StoryNode String String Int Int StoryTree Int Int StoryTree Int Int StoryTree Int Int



-- Getters (Very Gross Deconstruction) --
getoption :: StoryTree -> String
getoption (StoryLeaf option result reqresource reqamount) = option
getoption (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = option

getresult :: StoryTree -> String
getresult (StoryLeaf option result reqresource reqamount) = result
getresult (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = result

getreqresource :: StoryTree -> Int
getreqresource (StoryLeaf option result reqresource reqamount) = reqresource
getreqresource (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = reqresource

getreqamount :: StoryTree -> Int
getreqamount (StoryLeaf option result reqresource reqamount) = reqamount
getreqamount (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = reqamount

getchoice1 :: StoryTree -> StoryTree
getchoice1 (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = choice1

getchangeresource1 :: StoryTree -> Int
getchangeresource1 (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = changeresource1

getchangeamount1 :: StoryTree -> Int
getchangeamount1 (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = changeamount1

getchoice2 :: StoryTree -> StoryTree
getchoice2 (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = choice2

getchangeresource2 :: StoryTree -> Int
getchangeresource2 (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = changeresource2

getchangeamount2 :: StoryTree -> Int
getchangeamount2 (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = changeamount2

getchoice3 :: StoryTree -> StoryTree
getchoice3 (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = choice3

getchangeresource3 :: StoryTree -> Int
getchangeresource3 (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = changeresource3

getchangeamount3 :: StoryTree -> Int
getchangeamount3 (StoryNode option result reqresource reqamount choice1 changeresource1 changeamount1 choice2 changeresource2 changeamount2 choice3 changeresource3 changeamount3) = changeamount3
---------------------------------------------------------------------------------------------------------------



-- Resources Data Type -----------------------------------------------------------

-- Maybe we can make this slightly more elegant? This is basically a weird implementation of a fixed size [Int] at the moment
-- We could also change this to use strings instead of numbers for identification
-- Health, Time, Icepick

data Resources = Resources Int Int Int

getResource :: Resources -> Int -> Int
getResource (Resources health time icepick) 1 = health

getResource (Resources health time icepick) 2 = time

getResource (Resources health time icepick) 3 = icepick


changeResource :: Resources -> Int -> Int -> Resources
changeResource (Resources health time icepick) 1 change = (Resources (health+change) time icepick)

changeResource (Resources health time icepick) 2 change = (Resources health (time+change) icepick)

changeResource (Resources health time icepick) 3 change = (Resources health time (icepick+change))

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
      displaytreeoptions tree
      line <- getLineFixed
      if (line `elem` ["1","2","3"]) -- We need to go back to check these are actually met.
        then do
           play (movedown resources tree line)
        else return tree
	-- TODO: End on leaves


-- We move down to the selected option and modify the resources
movedown :: Resources -> StoryTree -> String -> (Resources, StoryTree)
movedown resources tree "1" = (traversechange resources tree "1", getchoice1 tree)

movedown resources tree "2" = (traversechange resources tree "2" , getchoice2 tree)

movedown resources tree "3" = (traversechange resources tree "3" , getchoice3 tree)
--------------------------------------------------------------------------------------



-- Displaying to terminal ------------------------------------------------------------

-- Print the result at the passed position
displaytreemessage:: StoryTree -> IO ()
displaytreemessage tree = do
    putStrLn(getresult tree)


-- Print the choices at the passed position
displaytreeoptions :: StoryTree -> IO ()
displaytreeoptions tree = do
    displaytreechoice(getchoice1 tree)
    displaytreechoice(getchoice2 tree)
    displaytreechoice(getchoice3 tree)


-- Display the choice of passed node
displaytreechoice :: StoryTree -> IO ()
displaytreechoice tree = do
    putStrLn(getoption tree)
--------------------------------------------------------------------------------------

     

-- Checking and Changing Resources ---------------------------------------------------

-- Modify a resource value based on an input number and a value increase or decrease
traversechange :: Resources -> StoryTree -> String -> Resources
traversechange resources tree "1" = resources
traversechange resources tree "2" = resources
traversechange resources tree "3" = resources
     

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

endnode = StoryLeaf "Any. This is an ending..." "The end." 0 0

firstchoice1 = StoryNode "1. Jump" "Jumping..." 0 0 endnode 0 0 endnode 0 0 endnode 0 0

firstchoice2 = StoryNode "2. Run" "Running..." 0 0 endnode 0 0 endnode 0 0 endnode 0 0

firstchoice3 = StoryNode "3. Climb" "Climbing..." 0 0 endnode 0 0 endnode 0 0 endnode 0 0

startnode = StoryNode "" "You are on everest... lets get the fuck down" 0 0 firstchoice1 0 0 firstchoice2 0 0 firstchoice3 0 0



main = do
    play (startingresources, startnode)

-- End of Basic Nodes -----------------------------------------------------
