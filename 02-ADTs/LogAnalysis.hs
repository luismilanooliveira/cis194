{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg = case (words msg) of
                "E":l:ts:m -> LogMessage (Error (read l)) (read ts) (unwords m)
                "I":ts:m   -> LogMessage Info (read ts) (unwords m)
                "W":ts:m   -> LogMessage Warning (read ts) (unwords m)
                _          -> Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t    = t
insert msg Leaf         = Node Leaf msg Leaf
insert msg (Node l m r) = case (compareMsg msg m) of
                               GT -> Node l m (insert msg r)
                               _  -> Node (insert msg l) m r


compareMsg :: LogMessage -> LogMessage -> Ordering
compareMsg (LogMessage _ ts _) (LogMessage _ ts' _) = compare ts ts'
compareMsg _ _                                      = error "not a log message"

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (l:ls) = insert l (build ls)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getError . inOrder . build . filter severeError

severeError :: LogMessage -> Bool
severeError (LogMessage (Error n) _ _) = n >= 50
severeError _                          = False

getError :: LogMessage -> String
getError (LogMessage _ _ m) = m
getError _ = undefined
