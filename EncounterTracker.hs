{- CPSC 312 WT1 2019 Project 1 - Haskell
Tabletop RPG Encounter Helper

By:
Devyani McLaren (Student #: 37276839)
Alexander Mountain (Student #: 60291549)
HOW TO PLAY:

- ghci
- load EncounterTracker
- main
Have fun! 

This tool is designed for game masters of Tabletop RPGs. It features the ability to add encounter descriptions including enemies and rewards.
You can run your added encounters and choose when you are ready to move onto the next. This is helpful for remembering minute details during the game.
Encounter lists can be saved as a text document if you wish to keep and work on them later. These can be loaded back in to be added to, deleted from or run at a later time.
-}

module Game where

import System.IO
import Text.Read
import Data.Char

-- ******************************* data types and basic getters for the types: *******************************

-- all rooms have a general description, and may also contain enemies and treasure!
data Room = Room Description Enemy Reward
    deriving(Show)

type Description = String -- Descripition of the room 
type Enemy = String -- Description of enemies 
type Reward = String -- Description of rewards in a room

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


-- deletes the first n elements of the list, useful for when we want to save the list of what we've worked on already. So that when we reload the list we're starting at the room we were about to enter. 
deleteFirstN :: [a] -> Int -> [a]
deleteFirstN [] _ = []
deleteFirstN lst 0 = lst
deleteFirstN lst n = deleteFirstN (tail lst) (n-1)



-- ****************************** some pre-defined variables to play the game *****************

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
r1 = Room descLvl1 enemyEasy reward1
r2 = Room descLvl2 enemyMedium reward2

--predefined room list
roomList = [r1, r2]

-- ******************************************* actual running of the game **********************************
-- Starts the encounter helper!
main = 
  do
    putStrLn("\nWelcome to the Tabletop RPG Encounter Helper!")
    putStrLn ("\nThis handy encounter runner is here to help you with all your favourite games that need to keep track of the levels you will be playing through! \nTo use, add all the levels (rooms) and their enemy and reward descriptions. Then start playing, it's that simple!\n\nTo begin, select from one of the options below and be sure to follow all the prompts.")
    start [] 0

-- runs the main menu. Everything gets done from here.
start:: [Room] -> Int -> IO ()
start lst r = 
    do --display introduction to the game including how to play, how to win etc.
        putStrLn ("\nWhat would you like to do?\nPress 0 to add rooms\nPress 1 to run your encounters\nPress 2 to delete a room from the room list\nPress 3 to quit and/or save the game")
        line <- getLine
        if (line == "0")
               then do
                    featureAdder lst
           else if (line == "1") then do 
             putStrLn "The adventure begins....\n"
             adventureTime 0 lst
           else if (line == "2") then do
             putStrLn "\nEnter the index of the list item you would like deleted"
             line <- getLine
             let n = read line :: Int
             let lst' = delete n lst
             putStrLn "\nYour list has been deleted"
             start lst' 0
             else if (line == "3") then do
                 quitGame lst r
         else do
           putStrLn "ERROR: please enter either 0, 1, 2, or 3\n"
           start lst 1

-- quits the game and gives the user an option to save their progress
quitGame lst n =
  do 
       putStrLn ("\nYour game has ended. Would you like to save your current progress, just so you can view your leftover rooms in a text file?\n0 = yes\n1 = no")
       ans <- getLine
       if (ans == "0") then do 
            putStrLn("\nSaved your current progress to file savegame.txt")
            let save = deleteFirstN lst n -- TODO 0 based indexing??
            putStrLn("Ending Game, thanks for playing!")
            writeFile "savegame.txt" (show save) 
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
    description <- getElemMistake 0 
    enemyD <- getElemMistake 1 
    reward <- getElemMistake 2 
    let room = Room description enemyD reward
    let lst' = addRoom lst room
    putStrLn ("\nYour room has been added! \nPress 0 to return to main menu \nPress 1 to add another room")
    line <- getLine
    if line == "0" then do
      start lst' 0
      else if line == "1" then do
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

