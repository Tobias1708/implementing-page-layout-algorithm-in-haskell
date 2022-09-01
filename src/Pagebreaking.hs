module Pagebreaking where

import qualified Graphics.DVI as DVI
import Utilities as UT
import qualified Linebreaking as LB
import Data.Int (Int32)
import Data.List (intersperse)
import Data.Char (chr)

data PageBreakError = NoInput  | NoLines | NoSolutionLooseness | NoRemainingActives
                    | LBNoInput | LBNoLines | LBNoSolutionLooseness | LBNoRemainingActives deriving Show

data DocType = Article | Book

data Position = Pos Height Vert 
data Height = Top | Bot
    deriving(Show,Eq)
data Vert = Le | Cent | Ri 
    deriving(Show,Eq)

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


-- computes the size of the Box where content can be placed (in pts), using the Margins data type
computeBoxSize :: Margins -> (Int, Int)
computeBoxSize (Margins l r t b) = computeBoxSize' l r t b
    where 
        computeBoxSize' :: Int -> Int -> Int -> Int -> (Int, Int)
        computeBoxSize' lMarg rMarg topMarg botMarg = (596 - (lMarg + rMarg),
                                              842 - (topMarg + botMarg))
        

-- makes a page(see definition above) out of the items from the linebreaking
preparePage :: [[LB.Item Char]] -> Int -> Float -> Page
preparePage [] _ _                   = P ([], 0)
preparePage items@(i:is) fs ilFactor = P (items,  (ilFactor - 1) * fromIntegral fs + fromIntegral fs)


-- decides where to break pages depending on the number of lines + interlineSpace  
simplePageBreaking :: Page -> Int -> [Page]
simplePageBreaking (P (ls, lh)) cHeight | length ls >= lineCount = P (take lineCount ls, lh)
                                          : simplePageBreaking (P (drop lineCount ls, lh)) cHeight
                                        | otherwise = [P (ls,lh)]
    where
        lineCount :: Int
        lineCount = floor ((65536 * fromIntegral cHeight) / lh)


-- page breaking algorithm, using the knuth-plass-linebreaking algorithm to make pages out of lines
knuthPlassPageBreaking :: [[LB.Item b]] -> [LB.Width] -> Int -> LB.Parameters -> Either PageBreakError (Either LB.LineBreakError [[LB.Item [LB.Item b]]])
knuthPlassPageBreaking lines pageHeights ils parameters'= Right (LB.knuthPlassLineBreaking' (buildLines ils lines) (Just [LB.Glue 1000 100000000 100000000]) pageHeights parameters')


-- adds changeable glue inbetween the lines
buildLines :: Int -> [b] -> [LB.Item b]
buildLines ils lines = intersperse (LB.Glue ils 100 100) (buildLines' lines) ++ [LB.Glue ils 100 100]
    where
        -- makes Boxes, which the algorithm can use, out of the lines
        buildLines' :: [a] -> [LB.Item a]
        buildLines'  = map (\x -> LB.Box (Just x) 655360)

-- reduces the complex data structure, the page-breaking algorithm gives as an output, by transforming the errors
-- from the linebreaking into errors of the pagebreaking
prune :: Either PageBreakError (Either LB.LineBreakError [[LB.Item [LB.Item Char]]]) -> Either PageBreakError [[LB.Item [LB.Item Char]]]
prune (Left e) = Left e
prune (Right (Left e)) = case e of
                            LB.NoInput -> Left LBNoInput
                            LB.NoLines -> Left LBNoLines
                            LB.NoSolutionLooseness -> Left LBNoSolutionLooseness
                            LB.NoRemainingActives -> Left LBNoRemainingActives
prune (Right (Right xs)) = Right xs


-- builds the internally used representation for Nodes, to later display the output in a DVI-File
dviNodeOutput :: DVI.Font -> [[LB.Item [LB.Item Char]]] -> [DVI.Node]
dviNodeOutput font =  map (dviPageNodeOutput font)
    where
        dviPageNodeOutput :: DVI.Font -> [LB.Item [LB.Item Char]] -> DVI.Node
        dviPageNodeOutput font' ((LB.Box (Just xs) bh) : (LB.Glue lh _ _):ls) =
            DVI.wrapNode
            $ DVI.BoxNode DVI.Vertical 0 0 0 (DVI.GlueSet 0 DVI.Fill DVI.Stretching)
            $ DVI.interlineGlue
             0
            (DVI.Glue (fromIntegral lh) (DVI.GlueSS 0 0 0 0) (DVI.GlueSS 0 0 0 0))
            (DVI.wrapNode $ DVI.PenaltyNode Nothing)
            $ concatMap (packItems font') [xs : extractItems ls]
        dviPageNodeOutput _ _ = error "something went wrong"

        numberConverter :: Int -> Float -> Int32
        numberConverter i f = fromIntegral $ fromEnum ((fromIntegral i :: Float) * f) :: Int32

        extractItems :: [LB.Item [LB.Item Char]] -> [[LB.Item Char]]
        extractItems [] = []
        extractItems [LB.Box (Just xs) _] = [xs]
        extractItems ((LB.Box (Just xs) _):ls) = xs : extractItems ls
        extractItems (_:ls ) = extractItems ls


pageNumbers :: DocType -> Position -> DVI.Font -> Float -> Int -> Int -> Bool -> [DVI.PageObjects]
pageNumbers Article p font ilf i offset sOnOff = pageNumbersArticle p font ilf i offset sOnOff
pageNumbers Book p font ilf i offset sOnOff = pageNumbersBook p font ilf i offset sOnOff

-- returns the internal representation of a pagenumber, including its position and value
pageNumbersArticle :: Position -> DVI.Font -> Float -> Int -> Int -> Bool -> [DVI.PageObjects]
pageNumbersArticle p font ilf i offset False | iOff < 1                            = []
                                       | offset < 1 && iOff < (10 + offset)  = [pnHelper p i 0]
                                       | offset > 0 && iOff < 10             = [pnHelper p iOff 0] 
                                       | offset < 1 && iOff >= (10 + offset) = [pnHelper p (i `mod` 10) 5,
                                                                                pnHelper p (i `div` 10) 0]
                                       | offset > 0 && iOff >= 10            = [pnHelper p (iOff `mod` 10) 5,
                                                                                pnHelper p (iOff `div` 10) 0]
                                                                                   where  
        iOff = i + offset                   
        pnHelper p' i' addX = DVI.shiftPage (DVI.points $ fromIntegral (fst (posToCoord p') + addX) , DVI.points $ fromIntegral $ snd $ posToCoord p' )
                 $ DVI.renderNode $ simpleDviNodeOutput font ilf [[LB.Box (Just (chr (i' + 48 ))) 0]]
pageNumbersArticle p font ilf i offset True  | offset < 0 && iOff < (1 + offset) || iOff < 1 = []
                                       | iOff < 10                                     = [pnHelper p iOff 0]
                                       | iOff >= 10                                    = [pnHelper p (iOff `mod` 10) 5,
                                                                                          pnHelper p (iOff `div` 10) 0]
                                                                                   where  
        iOff = i + offset                   
        pnHelper p' i' addX = DVI.shiftPage (DVI.points $ fromIntegral (fst (posToCoord p') + addX) , DVI.points $ fromIntegral $ snd $ posToCoord p' )
                 $ DVI.renderNode $ simpleDviNodeOutput font ilf [[LB.Box (Just (chr (i' + 48 ))) 0]]
pageNumbersArticle _ _ _ _ _ _ = []
 

pageNumbersBook :: Position -> DVI.Font -> Float -> Int -> Int -> Bool -> [DVI.PageObjects]
pageNumbersBook p@(Pos h Cent) font ilf i offset b = pageNumbersArticle p font ilf i offset b
pageNumbersBook (Pos h lor) font ilf i offset b | i `mod` 2 == 1  = pageNumbersArticle (Pos h Ri) font ilf i offset b
                                                    | otherwise = pageNumbersArticle (Pos h Le) font ilf i offset b

posToCoord :: Position -> (Int,Int)
posToCoord (Pos Top Ri)   = (482, 30)
posToCoord (Pos Top Cent) = (226,30)
posToCoord (Pos Top Le)   = (-30, 30) 
posToCoord (Pos Bot Ri)   = (482, -750)
posToCoord (Pos Bot Cent) = (226, -750)
posToCoord (Pos Bot Le)   = (-30, -750)
