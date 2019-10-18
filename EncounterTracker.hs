{- CPSC 312 WT1 2019 Project 1 - Haskell
Tabletop RPG Encounter Helper

By:
Devyani McLaren (Student #: 37276839)
Alexander Mountain (Student #: 60291549)
HOW TO USE:
This tool is designed for game masters of Tabletop RPGs. It features the ability to add encounter descriptions including enemies and rewards. You can run your added encounters and choose when you are ready to move onto the next. This is helpful for remembering minute details during the game. Encounter lists can be saved and reloaded if you would like to replay your game or start from where you ended. These can be loaded back in to be added to, deleted from or run at a later time.s


To begin: 

- ghci
- :load EncounterTracker.hs
- begin
Have fun! 



-}

{-# LANGUAGE 
    OverloadedStrings
  , DeriveGeneric
 #-}

module Game where
-- the below imports are needed for the save/load functionality of the program 
import Text.Read
import Data.Char
import Prelude hiding (readFile, writeFile)

import Data.Aeson
import Data.Text as T hiding (length, tail)
import Data.ByteString.Lazy as B hiding (putStrLn, length, tail, writeFile, readFile)
import Data.ByteString.Lazy.Char8 as BC hiding (putStrLn, length, tail)
import GHC.Generics

-- ******************************* data types and basic getters for the types: *******************************

-- all rooms have a general description, and may also contain enemies and treasure! The syntax seen is such that the data can be loaded into a JSON formatted textfile

data Room = Room
   { description :: String
   , enemy :: String
   , reward :: String
   } deriving (Show, Generic)


-- functions to encode and decode data (Room) as JSON/AESON 
instance FromJSON Room where
   parseJSON (Object v) =  Room <$> v .: "description" <*> v .: "enemy" <*> v .: "reward" 

instance ToJSON Room where
    toJSON (Room desc enem reward) = object ["description" .= desc, "enemy" .= enem, "reward" .= reward]
    
{-
type Description = String -- Descripition of the room 
type Enemy = String -- Description of enemies 
type Reward = String -- Description of rewards in a room
-}

getRDescription (Room d _ _ ) = d
getREnemy (Room _ e _ ) = e
getRReward (Room _ _ r ) = r

-- ******************************* functions to run the game: ***************************

-- useful function for debugging. Shows the rooms in a room list to command console.
showRooms :: [Room] -> String
showRooms [] = ""
showRooms (h:t) = (((((getRDescription h) ++ " ") ++ (getREnemy h)) ++ " " ++ (getRReward h)) ++ " : ") ++ (showRooms t)


-- Appends the given room onto the back of the given list.
addRoom :: [Room] -> Room -> [Room]
addRoom [] r = r : []
addRoom (h:t) r = h : addRoom t r

-- Removes the nth indexed item from the room list.
delete :: Int -> [Room] -> [Room]
delete n lst = deletehelper 0 n lst
deletehelper n0 n [] = []
deletehelper n0 n (h:t)
  | (n == n0) = t
  | otherwise = h : deletehelper (n0 + 1) n t


-- deletes the first n elements of the list, useful for when we want to save the list of what we've worked on already. So that when we reload the list we're starting at the room we were about to enter, is availalbe for ease of use, not implemented in our actual program  
deleteFirstN :: [a] -> Int -> [a]
deleteFirstN [] _ = []
deleteFirstN lst 0 = lst
deleteFirstN lst n = deleteFirstN (tail lst) (n-1)



-- ****************************** some pre-defined variables to play the game, which are useful for testing as well ************************

enemyEasy = "Alex"
enemyMedium = "Julian"

-- some rewards
reward1 = "A glittering pile of rubies, sapphires and gold lie before you."
reward2 = "Platinum!!!"

-- some descriptions
descLvl1 = "Room level 1 description"
descLvl2 = "Room level 2 description"
descLvl3 = "Room level 3 description"
descLvl4 = "Room level 4 description"

-- rooms
--r1 = Room descLvl1 enemyEasy reward1
--r2 = Room descLvl2 enemyMedium reward2

r1 = Room {description = descLvl1
          ,enemy = enemyEasy
          ,reward = reward2}

r2 = Room {description = descLvl2
          ,enemy = enemyMedium
          ,reward = reward1}


--predefined room list
roomList = [r1, r2]

-- ******************************************* actual running of the game **********************************
-- Starts the encounter helper!
begin = 
  do
    putStrLn("\nWelcome to the Tabletop RPG Encounter Helper!")
    putStrLn ("\n This handy encounter runner is here to help you with all your favourite games that need to keep track of the levels you will be playing through!\nTo use, add all the levels (rooms) and their enemy and reward descriptions. Then start playing, it's that simple!\nYou can also save your game so you can come back to it later!\nTo begin, select from one of the options below and be sure to follow all the prompts.")
    start [] 0

-- runs the main menu. Everything gets done from here. The added Int parameter is useful for when calling on deleteFirstN, which deletes the first n elements of a list
start:: [Room] -> Int -> IO ()
start lst r = 
    do --display introduction to the game including how to play, how to win etc.
        putStrLn ("\nWhat would you like to do?\nPress 0 to add rooms\nPress 1 to run your encounters\nPress 2 to delete a room from the room list\nPress 3 to quit and/or save the game\nPress 4 to reload the game you previously saved")
        line <- getLine
        if (line == "0") -- add features
               then do
                    featureAdder lst
           else if (line == "1") then do 
             putStrLn "The adventure begins....\n"
             adventureTime 0 lst
           else if (line == "2") then do -- deletes indexed items from the list
             putStrLn "\nEnter the index of the list item you would like deleted"
             line <- getLine
             let n = read line :: Int
             let lst' = delete n lst
             putStrLn "\nYour list has been deleted"
             start lst' 0
             else if (line == "3") then do -- quits the games and gives option to save
                 quitGame lst r
             else if (line == "4") then do -- loads the game previously saved 
                  load 
         else do -- error handling 
           putStrLn "ERROR: please enter either 0, 1, 2, or 3\n"
           start lst 1

-- loads the list of rooms saved as JSON/AESON format in the filename savegame.txt 
load =
   do
   putStrLn("Loading your previously saved game.")
   lst <- readFile "savegame.txt" -- reads file
   let new = decode lst -- decodes into the Room data type
   case new of -- need to account for the fact that decode returns Maybe 
      Nothing -> 
         error "Incorrect file format"
      Just n ->
         putStrLn("Your game has been loaded, have fun playing!") 
         start n 0

-- quits the game and gives the user an option to save their progress
quitGame:: [Room] -> Int -> IO()
quitGame lst n =
  do 
       putStrLn ("\nYour game has ended. Would you like to save your current encounter trakcer?\n0 = yes\n1 = no")
       ans <- getLine
       if (ans == "0") then do 
            putStrLn("\nSaved your current progress to file savegame.txt")
            putStrLn("\nEnding Game, thanks for playing!")
            writeFile "savegame.txt" (encode save) 
            return()
            else if (ans == "1") then do
               putStrLn("Ending Game, thanks for playing!") 
               return()
                else do 
                    putStrLn("ERROR: returing to main menu, don't worry you can still save your progress")
                    start lst 0

-- Accepts new rooms and adds them to the current room list. 
featureAdder:: [Room] -> IO()
featureAdder lst =
  do 
    putStrLn ("\nPlease follow the instructions to add a feature!")
    description <- getElemMistake 0 -- gets room description from user, allows them to reinput if they made a mistake
    enemyD <- getElemMistake 1  -- gets room enemies from user, allows them to reinput if they made a mistake
    reward <- getElemMistake 2  -- gets room rewards from user, allows them to reinput if they made a mistake
    let room = Room description enemyD reward
    let lst' = addRoom lst room
    putStrLn ("\nYour room has been added! \nPress 0 to return to main menu \nPress 1 to add another room")
    line <- getLine
    if (line == "0") then do
      start lst' 0
      else if (line == "1") then do
        featureAdder lst'
        else do
          putStrLn ("\nERROR: Unexpected input. Returning to main menu.")
          start lst' 0


-- a function which takes the adventure it's given and runs one encounter from the list of rooms at a time. n keeps track of what index of the list we're at 
adventureTime:: Int -> [Room] -> IO()
adventureTime n lst =
  do
    if (n < (length lst)) then do
      let room = (lst !! n)
      let description = getRDescription room
      let enemy = getREnemy room
      let reward = getRReward room
      putStrLn description
      putStrLn enemy
      putStrLn reward
      putStrLn "\nPress 0 to continue your quest\nPress 1 to go back to the main menu if you would like to take a break"
      line <- getLine
      if line == "0" then do
        adventureTime (n + 1) lst
        else if (line == "1") then do 
          start lst n 
            else do
              putStrLn "\nERROR: Please enter 0 when ready to continue"
              adventureWaiter n lst
     else do
        putStrLn "\nYour quest is complete!! \nBack to main menu."
        start lst 0


-- Waits for the correct input to continue reading out encounters. 
adventureWaiter :: Int -> [Room] -> IO()
adventureWaiter n lst = 
  do
    line <- getLine
    if line == "0" then do
      adventureTime (n + 1) lst
      else adventureWaiter n lst

-- gets the descriptions neccsary to make a room and allows the user the option to reinput a description if they would like to change it 
getElemMistake :: Int -> IO String 
getElemMistake 0 = -- gets the description, with option to rewrite if user made a mistake (i.e. spelling mistake)
  do
    putStrLn ("\nDescribe the surroundings for the encounter")
    description <- getLine
    putStrLn description
    putStrLn ("\nAre you happy with this description?\n Press 0 for yes\n Press 1 to rewrite it")
    ans <- getLine
    if (ans == "0") then do
      return description
    else if (ans == "1") then do
       getElemMistake 0 
      else do
       putStrLn ("\nERROR: Unexpected input Please reinput your description")
       getElemMistake 0 
      
getElemMistake 1 = -- gets the enemy, with option to rewrite if user made a mistake (i.e. spelling mistake)
  do
    putStrLn ("\nDescribe the enemies or lackthereof present in this encounter")
    enemy <- getLine
    putStrLn enemy
    putStrLn ("\nAre you happy with this description?\n Press 0 for yes\n Press 1 to rewrite it")
    ans <- getLine
    if (ans == "0") then do
      return enemy
    else if (ans == "1") then do
       getElemMistake 0 
      else do
       putStrLn ("\nERROR: Unexpected input Please reinput your enemy")
       getElemMistake 0 
getElemMistake 2 = -- gets the reward, with option to rewrite if user made a mistake (i.e. spelling mistake)
  do
    putStrLn ("\nFinally, describe any treasure or loot that can be found in this encounter")
    reward <- getLine
    putStrLn reward
    putStrLn ("\nAre you happy with this description?\n Press 0 for yes\n Press 1 to rewrite it")
    ans <- getLine
    if (ans == "0") then do
      return reward
    else if (ans == "1") then do
       getElemMistake 0 
      else do
       putStrLn ("\nERROR: Unexpected input Please reinput your reward")
       getElemMistake 0 

