{-# LANGUAGE BlockArguments #-}
module PageExamples where

import Graphics.DVI as DVI
import Linebreaking as LB
import Utilities as UT
import TestText as TT
import qualified Linebreaking as LB
import Pagebreaking as PB
import GHC.Word (Word32)
import Markdown as MD
import Data.Char
import qualified Pagebreaking as PE
import Data.Either

defaultParameters :: LB.Parameters
defaultParameters = Parameters 10000 10000 infinity infinity

defaultLines :: [Int]
defaultLines = [ fromIntegral $ points (fromIntegral $ fst $ computeBoxSize' defaultMargins) ]

-- experimental
defaultPageHeights :: [Int]
defaultPageHeights = concat $ replicate 12 [fromIntegral $ points (46 * 10 + 45 * 15)]

defaultFont :: IO Font
defaultFont = loadFont (Right defaultFontSize)
                       "C:/texlive/2021/texmf-dist/fonts/tfm/public/baskervillef/BaskervilleF-Regular.tfm"

defaultFontSize :: Word32
defaultFontSize = 65536 * 10

defaultInterlineFactor :: Float
defaultInterlineFactor = 1.2

defaultInterlineSpace :: Int
defaultInterlineSpace = 65536 * 15

defaultPageOffset :: Int 
defaultPageOffset = 2

startOnNonFirstPage :: Bool 
startOnNonFirstPage = True

-- default 72 72 64 64 
defaultMargins :: PB.Margins
defaultMargins = Margins 72 72 64 64


defaultLinebreaker :: [Item Char] -> [Int] -> LB.Parameters -> Font -> String -> IO ()
defaultLinebreaker items lines' parameters' font fileName = do
  let
    result = LB.processOutput (Just '-') items $ crashErrorHandling $ LB.knuthPlassLineBreaking items lines' parameters'
    node = simpleDviNodeOutput font defaultInterlineFactor result
  singlePageDocument node $ "./files/" ++ fileName

-- first attempt to make multi-page documents, by building them by hand
multiPageLinebreaker :: [Item Char] -> [Int] -> LB.Parameters -> Font -> String -> IO()
multiPageLinebreaker items lines' parameters' font fileName = do
  let
    -- result = list of lines (line == list of items)
    result = concat $ replicate 7 $ LB.processOutput (Just '-') items $ crashErrorHandling $ LB.knuthPlassLineBreaking items lines' parameters'
    pages = simplePageBreaking (preparePage result (fromIntegral defaultFontSize) defaultInterlineFactor) (snd $ computeBoxSize' defaultMargins)
    node1 = simpleDviNodeOutput font defaultInterlineFactor (getLines $ pages !! 0)
    node2 = simpleDviNodeOutput font defaultInterlineFactor (getLines $ pages !! 1)
  print (length $ simplePageBreaking (preparePage result (fromIntegral defaultFontSize) defaultInterlineFactor) (snd $ computeBoxSize' defaultMargins))
  createDVI ("./files/" ++ fileName) 1000 dviUnitsTeX
   >>= shipOut . Page (pageNum (1 :: Int))
                      (shiftPage (points (fromIntegral $ PB.getLeftMargin defaultMargins), points (fromIntegral $ PB.getTopMargin defaultMargins)) $ renderNode node1)
   >>= shipOut . Page (pageNum (2 :: Int)) (shiftPage (fromIntegral $ PB.getLeftMargin defaultMargins, -nodeHeight node2) $ renderNode node2)
  >>= finishDVI
  where
    pageContent font ilf margs p = shiftPage (points (fromIntegral $ PB.getLeftMargin margs), points (fromIntegral $ PB.getTopMargin margs)) $
                                                        renderNode (simpleDviNodeOutput font ilf (getLines p))

-- creates multi-page documents out of the result of the simple page-breaking algorithm
multiPageLinebreaker' :: [Item Char] -> [Int] -> LB.Parameters -> Font -> String -> IO()
multiPageLinebreaker' items lines' parameters' font fileName = do
  let
    -- result = concat $ replicate 7 $ LB.processOutput (Just '-') items $ crashErrorHandling $ LB.knuthPlassLineBreaking items lines' parameters'
    result = LB.processOutput (Just '-') items $ crashErrorHandling $ LB.knuthPlassLineBreaking items lines' parameters'
    pages = simplePageBreaking (preparePage result (65536 * 10) defaultInterlineFactor) (snd $ computeBoxSize' defaultMargins)
  createDVI ("./files/" ++ fileName) 1000 dviUnitsTeX
    >>= mPLhelper 1 pages font defaultInterlineFactor defaultMargins
    >>= finishDVI
  where
    mPLhelper :: Int -> [PB.Page] -> Font -> Float -> PB.Margins -> DocStat -> IO DocStat
    mPLhelper _ [] _ _ _ _= error "no valid document"
    mPLhelper i [p] font iLFactor margs ds = (shipOut . Page (pageNum (i :: Int)) (concat $ pageContent i font iLFactor margs p)) ds
    mPLhelper i (p:ps) font iLFactor margs ds = (shipOut . (Page (pageNum (i :: Int)) (concat $ pageContent i font iLFactor margs p)) ) ds
                                                        >>= (mPLhelper (i + 1) ps font iLFactor margs)
    pageContent i font ilf margs p = [shiftPage (points (fromIntegral $ PB.getLeftMargin margs), points (fromIntegral $ PB.getTopMargin margs)) $
                                                        renderNode (simpleDviNodeOutput font ilf (getLines p)),
                                    shiftPage (points $ fromIntegral 226, points $ fromIntegral 30) $ renderNode $ simpleDviNodeOutput font ilf [[LB.Box (Just (chr (i + 48))) 0]]]


-- creates a document using the non-knuth-plass, simple pagebreaking algorithm
ownDoc :: IO ()
ownDoc = do
  font <- defaultFont
  -- content <- MD.loadFile "test.md"
  -- content <- MD.splitLines content
  let
    items = stringToItems font (TT.mobyDickChapterOne ++ TT.mobyDickChapterTwo ++ TT.mobyDickChapterThree)  defaultSpaceFactors False
  multiPageLinebreaker' items defaultLines defaultParameters font "ownDoc.dvi"

-- used for debugging, prints the result of the algorithm
examplePB :: IO()
examplePB = do
    font <- defaultFont
    let
      items = stringToItems font (TT.mobyDickChapterOne ++ TT.mobyDickChapterTwo ++ TT.mobyDickChapterThree)  defaultSpaceFactors False
    print $ prune $ PB.knuthPlassPageBreaking (LB.knuthPlassLineBreaking' items (Just '-') defaultLines defaultParameters) defaultPageHeights defaultInterlineSpace defaultParameters

--  first attempt to create a document using the page-breaking algorithm and the DVI-processing, printing just one page
makePage :: Either PageBreakError [[Item [Item Char]]] -> Font -> IO()
makePage pages font = createDVI ("./files/" ++ "something.dvi") 1000 dviUnitsTeX
                      >>= shipOut . Page (pageNum (1 :: Int))
                        (shiftPage (points (fromIntegral $ PB.getLeftMargin defaultMargins), points (fromIntegral $ PB.getTopMargin defaultMargins)) $ renderNode (PB.dviNodeOutput font (fromRight [] pages) !! 1))
                      >>= finishDVI


-- creates a document, using the pagebreaking algorithm
kpbDocument :: IO()
kpbDocument = do
      font <- defaultFont
      let
        items = stringToItems font (TT.mobyDickChapterOne ++ TT.mobyDickChapterOne ++ TT.mobyDickChapterTwo ++ TT.mobyDickChapterThree ++ TT.mobyDickChapterFour)  defaultSpaceFactors False
        pages = prune $ PB.knuthPlassPageBreaking (LB.knuthPlassLineBreaking' items (Just '-') defaultLines defaultParameters) defaultPageHeights defaultInterlineSpace defaultParameters
      -- print pages
      multiPageLinebreaker'' pages font "kpb_document.dvi"

--  alternative version of multiPageLinebreaker' for the page-breaking algorithm instead of the simple one
multiPageLinebreaker'' :: Either PageBreakError [[Item [Item Char]]] -> Font -> String -> IO()
multiPageLinebreaker'' (Right pages) font fileName = do
  createDVI ("./files/" ++ fileName) 1000 dviUnitsTeX
  >>= mPLhelper' 1 pages font
  >>= finishDVI
  where
    mPLhelper' :: Int -> [[Item [Item Char]]] -> Font -> DocStat -> IO DocStat
    mPLhelper' _ [] _ _= error "no valid document"
    mPLhelper' i [p] font ds = (shipOut . Page (pageNum (i :: Int)) (concat $ pageContent' i font p)) ds
    mPLhelper' i (p:ps) font ds | i <= length defaultPageHeights = (shipOut . (Page (pageNum (i :: Int)) (concat $ pageContent' i font p))) ds
                                          >>= (mPLhelper' (i+1) ps font)
                                | otherwise = (shipOut . (Page (pageNum (i :: Int)) (concat $ pageContent' i font p))) ds
    pageContent' i font p = shiftPage (points (fromIntegral $ PB.getLeftMargin defaultMargins), points (fromIntegral $ PB.getTopMargin defaultMargins)) (renderNode (PB.dviNodeOutput font pages !! (i-1)))
                             : PB.pageNumbers (Pos Top Ri) font defaultInterlineFactor i defaultPageOffset startOnNonFirstPage
multiPageLinebreaker'' _ _ _ = error "no valid document"