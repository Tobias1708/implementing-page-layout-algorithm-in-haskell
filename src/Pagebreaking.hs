module Pagebreaking where

import qualified Graphics.DVI as DVI
import Utilities as UT
import qualified Linebreaking as LB
import Data.Int
import Data.List
import Linebreaking (knuthPlassLineBreaking)

data PageBreakError = NoInput  | NoLines | NoSolutionLooseness | NoRemainingActives deriving Show

--  data model representing the margins of a Page (unit: pts)
data Margins  = Margins {
    leftMargin :: Int,
    rightMargin :: Int,
    topMargin :: Int,
    bottomMargin :: Int
}

-- includes computation for the shift when building the pages
getLeftMargin :: Margins -> Int
getLeftMargin (Margins l _ _ _) | l > 72 = l
                                | otherwise = l - 72

getRightMargin :: Margins -> Int
getRightMargin (Margins _ r _ _) = r

-- includes computation for the shift when building the pages
getTopMargin :: Margins -> Int
getTopMargin (Margins _ _ t _) | t > 64 = - (t - 64)
                               | otherwise = 64 - t

getBotMargin :: Margins -> Int
getBotMargin (Margins _ _ _ b) = b

-- represents a page consisting of lines
newtype Page = P ([[LB.Item Char]], Float)
    deriving(Show)

getLines :: Page -> [[LB.Item Char]]
getLines (P (ls, _)) = ls


-- computes the size of the Box where content can be placed (in pts)
computeBoxSize :: Int -> Int -> Int -> Int -> (Int, Int)
computeBoxSize lMarg rMarg topMarg botMarg = (596 - (lMarg + rMarg),
                                              842 - (topMarg + botMarg))
-- computes the size of the Box where content can be placed (in pts), using the Margins data type
computeBoxSize' :: Margins -> (Int, Int)
computeBoxSize' (Margins l r t b) = computeBoxSize l r t b


-- makes a page(see definition above) out of the items from the linebreaking
preparePage :: [[LB.Item Char]] -> Int -> Float -> Page
preparePage [] _ _ = P ([], 0)
preparePage items@(i:is) fs ilFactor = P (items,  (ilFactor - 1) * fromIntegral fs + fromIntegral fs)


-- decides where to break pages depending on the number of lines + interlineSpace  
simplePageBreaking :: Page -> Int -> [Page]
simplePageBreaking (P (ls, lh)) cHeight | length ls >= lineCount = P (take lineCount ls, lh)
                                          : simplePageBreaking (P (drop lineCount ls, lh)) cHeight
                                        | otherwise = [P (ls,lh)]
    where
        lineCount :: Int
        lineCount = floor ((65536 * fromIntegral cHeight) / lh)

knuthPlassPageBreaking :: Either LB.LineBreakError [[LB.Item b]] -> [LB.Width] -> LB.Parameters -> Either PageBreakError (Either LB.LineBreakError [[LB.Item [LB.Item b]]])
knuthPlassPageBreaking (Left _) _ _ = Left NoInput
knuthPlassPageBreaking (Right lines) pageHeights parameters'= Right (LB.knuthPlassLineBreaking' (buildLines' lines) (Just [LB.Glue 1000 100000000 100000000]) pageHeights parameters')

buildLines :: [a] -> [LB.Item a]
buildLines  = map (\x -> LB.Box (Just x) 655360)

buildLines' :: [b] -> [LB.Item b]
buildLines' lines = intersperse (LB.Glue 655360 100 100) (buildLines lines)

prune :: Either PageBreakError (Either LB.LineBreakError [[LB.Item [LB.Item Char]]]) -> Either PageBreakError [[LB.Item [LB.Item Char]]]
prune (Left e) = Left e
prune (Right (Left e)) = case e of
                            LB.NoInput -> Left NoInput
                            LB.NoLines -> Left NoLines
                            LB.NoSolutionLooseness -> Left NoSolutionLooseness
                            LB.NoRemainingActives -> Left NoRemainingActives
prune (Right (Right xs)) = Right xs


