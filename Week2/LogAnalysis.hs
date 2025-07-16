{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Week2.LogAnalysis where
import Week2.Log

{-Exercise 1 The first step is figuring out how to parse an individual
message. Define a function
parseMessage :: String -> LogMessage
which parses an individual line from the log file. For example,
-}

parseMessage :: String -> LogMessage
parseMessage str = let wordlist = words str in 
    case wordlist of
        ("I":ts:rest) -> LogMessage Info (read ts) (unwords rest)
        ("W":ts:rest) -> LogMessage Warning (read ts) (unwords rest)
        ("E":lvl:ts:rest) -> LogMessage (Error (read lvl)) (read ts) (unwords rest)
        _ -> Unknown (unwords wordlist)

parseFile :: String -> [LogMessage]
parseFile str = parseList (lines str) where
  parseList [] = []
  parseList (x:l) = parseMessage x : parseList l

{-Exercise 2 Define a function
insert :: LogMessage -> MessageTree -> MessageTree
which inserts a new LogMessage into an existing MessageTree, pro-
ducing a new MessageTree. insert may assume that it is given a
sorted MessageTree, and must produce a new sorted MessageTree
containing the new LogMessage in addition to the contents of the
original MessageTree.
However, note that if insert is given a LogMessage which is
Unknown, it should return the MessageTree unchanged.
-}

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert inputMessage Leaf = Node Leaf inputMessage Leaf
insert inputMessage (Node t_left nodeMessage t_right )
  | inputMessage `isLess` nodeMessage = Node (insert inputMessage t_left ) nodeMessage t_right
  | otherwise = Node t_left nodeMessage (insert inputMessage t_right) 

isLess :: LogMessage -> LogMessage -> Bool
isLess (Unknown _) _ = False
isLess _ (Unknown _) = True
isLess (LogMessage _ a _) (LogMessage _ b _) = a < b

{-Exercise 3 Once we can insert a single LogMessage into a MessageTree,
we can build a complete MessageTree from a list of messages. Specifi-
cally, define a function
build :: [LogMessage] -> MessageTree
which builds up a MessageTree containing the messages in the list,
by successively inserting the messages into a MessageTree (beginning
with a Leaf).
-}

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


{-Exercise 4 Finally, define the function
inOrder :: MessageTree -> [LogMessage]
which takes a sorted MessageTree and produces a list of all the
LogMessages it contains, sorted by timestamp from smallest to biggest.
-}

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t_l msg t_r) = inOrder t_l ++ (msg: inOrder t_r)


{-xercise 5 Now that we can sort the log messages, the only thing
left to do is extract the relevant information. We have decided that
“relevant” means “errors with a severity of at least 50”.
Write a function
whatWentWrong :: [LogMessage] -> [String]
which takes an unsorted list of LogMessages, and returns a list of the
messages corresponding to any errors with a severity of 50 or greater,
sorted by timestamp. (Of course, you can use your functions from the
previous exercises to do the sorting.)
-}


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = filterSeverity (sortMessages x)

filterSeverity :: [LogMessage]->[String]
filterSeverity [] = []
filterSeverity (x:l)
  | getSeverity x < 50 = filterSeverity l
  | otherwise = getString x : filterSeverity l

getSeverity :: LogMessage -> Int
getSeverity (LogMessage (Error x) _ _) = x
getSeverity _ = -1

getString :: LogMessage -> String
getString (Unknown x) = x
getString (LogMessage _ _ x) = x

sortMessages :: [LogMessage]->[LogMessage]
sortMessages = inOrder . build


