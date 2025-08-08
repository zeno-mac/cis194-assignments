module Main where

import JoinList
import Editor
import JoinList
import Scrabble
import Sized
import Buffer
import StringBuffer

buffJoin :: JoinList (Score, Size) String
buffJoin = fromString $ unlines
    [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]


buffString :: String
buffString = unlines 
    [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main = runEditor editor buffJoin