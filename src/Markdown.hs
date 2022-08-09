module Markdown where

import Data.List

-- reads a file and checks whether it is a MarkDown-File
loadFile :: String -> IO String
loadFile fileName | [last $ init $ init fileName, last $ init fileName, last fileName]== ".md"  = readFile("./files/" ++ fileName)
                  | otherwise = error "please enter a valid fileName (.md)"

-- splits the content of the MarkDown-File into its lines
splitLines :: String -> IO [String]
splitLines str = do return $ lines str 



