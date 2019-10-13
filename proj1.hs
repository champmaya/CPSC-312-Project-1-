{- CPSC 312 WT1 2019 Project 1 - Haskell
Text Based Randomly Generated RPG 
By:
Devyani McLaren (Student #: 37276839)
Alexander Mountain (Student #: 604...???)

TODO/NOTES/COMMENTS:

- Alex created some data types and basic get functions associated with them
- Dev did some more framework stuff as well as worked on creating a random level generator function(s)  
- Do we want to consider giving the enemies an added paramter for having a name? ( I already did it, lmk if you want to change it back) 
- Also, do you think we should have a room have a list of rewards? that way a room could give a health and score boost if it wanted to 
- I can't remember exactly, did we want it such that the player can go back to the previous room??? 
- For the list of exits index 0 = room to back to go, 1 = room left, 2 = room right, 3 = room forward  
- Some things todo (other than things listen TODO below)
    0. Lots of debugging & figuring out converting IO -> not IO (Dev will work on this but any immediate thoughts that may spring to your mind might help) 
    1. Create a function that will run a level (i.e. display level description & options, run a player attacking an enemy, run a player receiving a reward, having the player choose where they want to go next and adding that randomly generated level to the list of exits in the current room they're in 
    2. Create a function that will run the pre-made game we have (which would run levels 1-4 and then all randomly genereated levels) 


To run it put: (this techincally doesn't work yet :P ) 
- ghci
- load proj1
- start 
 
-}



module Game where

import System.IO
import Text.Read   --(readMaybe)
--import Data.Maybe   (fromJust)
import System.Random (randomRIO)

-- ******************************* data types and basic getters for the types: *******************************

-- all rooms contain enemies 
data Room = Room  Description Enemy Reward Exits -- changed [Exit] -> [Room]
     deriving(Show)

type Description = String
type Exits = [Room]

getRoomDescription (Room d _ _ _) = d
getRoomEnemy (Room _ e _ _) = e
getRoomReward (Room _ _ r _) = r
getRoomExits (Room _ _ _ exts) = exts

data Player = Player Health Attack Score  -- player has health, attack strength and score values.
     deriving(Show)

-- we may want to consider changing these values to doubles, but that's some down the line stuff to consider

type Health = Integer
type Attack = Integer
type Score = Integer

getPAttack (Player _ a _) = a
getPHealth (Player h _ _) = h
getPScore (Player _ _ s) = s

data Enemy = Maybe -- Nothing  <--- this is showing an error for some reason , this is such that there may not be an enemy in the room correct? 
           | Enemy Name Health Attack  -- enemies have a name and  set attack strength and health values
         deriving(Show)

type Name = String 

-- rewards can be of three subtypes, attackboost, healthboost, and scoreboost.
data Reward = AtkBoost Int  -- increases damage dealt by the player by Int
            | HealthBoost Int  -- increases player health by Int
            | ScoreBoost Int   -- increases player score by Int
         deriving(Show)

data Maybe a = Nothing
               |Just a
           deriving(Show) 
           

-- ******************************* functions to run the game: *******************************

-- displays the options the player can do
showOpts :: Int -> String
showOpts 0 = (show "Beginning of game options...") -- TODO
showOpts 1 = (show "Create your own game options...") -- TODO
showOpts 2 = (show "Playing actual game options...") -- TODO

-- generates a level with enemy and reward  both randomly chosen
-- need to figure out all the IO things (it's given IO type errors) 
generateLvlRand :: [Enemy] -> [Reward] -> Room -- TODO
generateLvlRand enemies rewards = 
   do 
       let enemy = pick enemies
       let reward = pick rewards
       let desc = "Generic Description" -- TODO create a function that will generate a description of the room given the randomly picked enemy & reward
       let numExits = pick [1, 2, 3] -- we can use this number to let the player know how many exit options they have after defeating an enemy and receiving a reward 
       let exits = []
       let roomRand = Room desc enemy reward exits 
       return roomRand

-- picks a random element from a list, unsure what to do about the IO a thing tho 
-- help from this website: https://rosettacode.org/wiki/Pick_random_element#Haskell
pick :: [a] -> IO a -- TODO
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)
   

--runPreMadeGame TODO 



-- ****************************** some pre-defined variables to play the game *****************
-- some enemies
noEnemy = Enemy "Devyani" 0 0  
enemyEasy = Enemy "Alex" 2 1 -- health = 2 attack = 1
enemyMedium = Enemy "Julian"  5 3
enemyHard = Enemy "Sowmya" 7 6 

listOfEnemy = [noEnemy, enemyEasy, enemyMedium, enemyHard]


-- some rewards
smallRewardAtk = AtkBoost 1
smallRewardHelt = HealthBoost 1
smallRewardScore = ScoreBoost 1

medRewardAtk = AtkBoost 3
medRewardHelt = HealthBoost 3
medRewardScore = ScoreBoost 3

hiRewardAtk = AtkBoost 6
hiRewardHelt = HealthBoost 6
hiRewardScore = ScoreBoost 6


listOfReward = [smallRewardAtk, smallRewardHelt, smallRewardScore, medRewardAtk, medRewardHelt, medRewardScore, hiRewardAtk, hiRewardHelt, hiRewardScore] 


-- some descriptions
descLvl1 = "Room level 1 description"
descLvl2 = "Room level 2 description"
descLvl3 = "Room level 3 description"
descLvl4 = "Room level 4 description"

-- some rooms (levels):
fakelvl = Room "" noEnemy smallRewardAtk []
lvl1 = Room descLvl1 noEnemy smallRewardAtk [fakelvl, lvl2]
lvl2 = Room descLvl2 enemyEasy smallRewardHelt [lvl1, lvl3, lvl4]
lvl3 = Room descLvl3 enemyEasy smallRewardScore [lvl2]
lvl4 = Room descLvl4 enemyMedium medRewardScore [lvl2]
-- I was thinking these could be our baseline levels that every pre-made game would start with and after level4 we would then start randomly generating levels but we talk about this later

-- ******************************************* actual running of the game **********************************

-- function to begin game. (Starts the game) 
--start = -- TODO
     -- putStrLn("...") will display introduction to the game including how to play, how to win etc. 
     -- putStrLn ("\n\nWhat would you like to do?")
     -- show the beginning of game options
     -- receive answer
     -- if (play pre-made game then do ___, if create own game then ______, otherwise (if quit) then _______)








