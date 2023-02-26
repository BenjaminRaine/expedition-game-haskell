import System.IO

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
movedown tree "1" = getchoice1 tree

movedown tree "2" = getchoice2 tree

movedown tree "3" = getchoice3 tree


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
     

-- Modify a resource value based on an input number and a value increase or decrease
resourcechange :: StoryTree -> IO ()
resourcechange tree = do
    putStrLn("Need to implement resource management")
     

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






-- Basic Nodes for Traversal Test -----------------------------------------
endnode = StoryLeaf "Any. This is an ending..." "The end." 0 0

firstchoice1 = StoryNode "1. Jump" "Jumping..." 0 0 endnode 0 0 endnode 0 0 endnode 0 0

firstchoice2 = StoryNode "2. Run" "Running..." 0 0 endnode 0 0 endnode 0 0 endnode 0 0

firstchoice3 = StoryNode "3. Climb" "Climbing..." 0 0 endnode 0 0 endnode 0 0 endnode 0 0

startnode = StoryNode "" "You are on everest... lets get the fuck down" 0 0 firstchoice1 0 0 firstchoice2 0 0 firstchoice3 0 0



main = do
    play startnode

-- End of Basic Nodes -----------------------------------------------------
