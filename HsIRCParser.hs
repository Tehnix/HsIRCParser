{-| Parse IRC output.

The main function of this module is the `parse` function. It takes in some
IRC output, and generates a tuple/dict/list/object (the kwarg `output` in the `parse` function) looking like the following:

From: :irc.codetalk.io 332 Innocence #lobby :Let's all have great fun! :D
To: (
    'irc.codetalk.io', -- server
    '#lobby', -- channel
    Nothing, -- recipient
    Nothing, -- user
    '332', -- type
    'Let us all have great fun! :D' -- msg
)
    
This will indicate that a topic has been found for a channel. An example of 
a user action would be:

From: :Tehnix!Tehnix@ghost-EC31B3C1.rdns.scalabledns.com NICK :BlaBliBlu
To: (
    Nothing, -- server
    Nothing, -- channel
    Nothing, -- recipient
    ('Tehnix', 'Tehnix', 'ghost-EC31B3C1.rdns.scalabledns.com'), -- user
    'NICK', -- type
    'BlaBliBlu' -- msg
)

Here, the user changed his nickname from 'Tehnix' to 'BlaBliBlu'.
-}
module HsIRCParser.HsIRCParser where

import Data.Char


-- Split a string on a given delimter
split :: String -> Char -> [String]
split str delim = let (start, end) = break (== delim) str
    in start : if null end then [] else split (tail end) delim  

-- Split the string on : and remove the first empty element of the list
tokenize ::  String -> [String]
tokenize s = tail $ split s ':'

-- Get the nickname
getNickname ::  [String] -> String
getNickname t = head $ split (head t) '!'

-- Strip the nickname of the @~!& chars in front of ops etc
strippedNickname :: String -> String
strippedNickname s = if head s `elem` "@~!&" then tail s else s

-- Get the hostname
getHostname :: [String] -> String
getHostname t = head $ words $ split (split (head t) '!'!!1) '@'!!1

-- Get the username
getUsername :: [String] -> String
getUsername t = head $ split (split (head t) '!'!!1) '@'

-- Get the channel
getChannel :: [String] -> String
getChannel [] = ""
getChannel w = if getChannel' (head w) then head w else getChannel $ tail w

getChannel' :: String -> Bool
getChannel' ('#':_) = True
getChannel' _ = False

-- Get the action
getAction :: [String] -> String
getAction t = words (head t)!!1

-- Check if there is an IRC code present
isCode :: [String] -> Bool
isCode t = let w = words (head t) in length w > 1 && all isDigit (w!!1)

-- Grab the IRC code from the output
getCode :: [String] -> String
getCode t = words (head t)!!1

parse ::  String -> IO ()
parse s = let
    t = tokenize s
    nickname = strippedNickname $ getNickname t
    hostname = getHostname t
    username = getUsername t
    userinfo = (nickname, username, hostname)
    action = getAction t
    in print (action, userinfo)

-- main :: IO ()
-- main = parse ":Tehnix!Tehnix@ghost-EC31B3C1.rdns.scalabledns.com NICK :BlaBliBlu"

