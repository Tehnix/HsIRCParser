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
module IRCParser where


split :: String -> Char -> [String]
split str delim = let (start, end) = break (== delim) str
    in start : if null end then [] else split (tail end) delim  

tokenize ::  String -> [String]
tokenize s = tail $ split s ':'

getNickname ::  [String] -> String
getNickname t = head $ split (head t) '!'

strippedNickname :: String -> String
strippedNickname s = if head s `elem` "@~!&" then tail s else s

getHostname :: [String] -> String
getHostname t = head $ words $ split (split (head t) '!'!!1) '@'!!1

getUsername :: [String] -> String
getUsername t = head $ split (split (head t) '!'!!1) '@'

getAction :: [String] -> String
getAction t = (words (head t))!!1

-- Check if there is an IRC code present
isCode :: [String] -> Bool
isCode t = let w = words (head t) in if length w > 1 then all isDigit (w!!1) else False

-- Grab the IRC code from the output
getCode :: [String] -> String
getCode t = (words (head t))!!1

parse s = do
    t <- return $ tokenize s
    nickname <- return $ strippedNickname $ getNickname t
    hostname <- return $ getHostname t
    username <- return $ getUsername t
    userinfo <- return $ (nickname, username, hostname)
    action <- return $ getAction t
    print (action, userinfo)

main = parse ":Tehnix!Tehnix@ghost-EC31B3C1.rdns.scalabledns.com NICK :BlaBliBlu"

