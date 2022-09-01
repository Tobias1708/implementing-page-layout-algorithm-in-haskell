{-# LANGUAGE BlockArguments #-}
module PageExamples (ownDoc,kpbDoc,defaultSettings,mdSettings, dgSettings, meSettings) where

import Graphics.DVI as DVI
import qualified Linebreaking as LB
import Utilities as UT
import TestText as TT
import Pagebreaking as PB
import GHC.Word (Word32)
import Data.Either (fromRight)
import qualified Pagebreaking as PB
import Text.Show (Show)

data Settings = Settings {
  lbParams :: LB.Parameters,
  pbParams :: LB.Parameters,
  font :: IO Font,
  fontSize :: Word32,
  ilFactor :: Float,
  spaceFactors :: SimpleSpaceFactors,
  numberPosition :: PB.Position,
  pageOffset :: Int,
  startOnOffset :: Bool,
  lineWidth :: [Int],
  pageHeight :: [Int],
  margins :: PB.Margins,
  docType :: PB.DocType 
}  
-- ####### the following section contains all settings that can be made for the page layout algorithm ######
defaultSettings :: Settings
defaultSettings = Settings {
                   lbParams = defaultLBParameters,
                   pbParams = defaultPBParameters,
                   font = defaultFont,
                   fontSize = defaultFontSize,
                   ilFactor = defaultInterlineFactor,
                   spaceFactors = defaultSpaceFactors,
                   numberPosition = defaultNumberPosition,
                   pageOffset = defaultPageOffset,
                   startOnOffset = True,
                   lineWidth = defaultLines,
                   pageHeight = defaultPageHeights,
                   margins = defaultMargins,
                   docType = PB.Book}

mdSettings :: Settings
mdSettings = defaultSettings{spaceFactors=UT.SimpleSpaceFactors (1.0/3.0) (1.0/6.0) (1.0/9.0) 4.0, numberPosition=PB.Pos Bot Cent} 
dgSettings :: Settings
dgSettings = defaultSettings{pageHeight = take 6 defaultPageHeights, ilFactor=1.8, pageOffset = 0}
meSettings :: Settings
meSettings = defaultSettings{pageHeight = take 7 defaultPageHeights, font= loadFont (Right defaultFontSize) "C:/texlive/2021/texmf-dist/fonts/tfm/public/baskervillef/BaskervilleF-Italic.tfm"}


defaultLBParameters :: LB.Parameters
defaultLBParameters = LB.Parameters 10000 10000 LB.infinity LB.infinity

defaultPBParameters :: LB.Parameters
defaultPBParameters = LB.Parameters 10000 10000 LB.infinity LB.infinity

defaultFont :: IO Font
defaultFont = loadFont (Right defaultFontSize)
                       "C:/texlive/2021/texmf-dist/fonts/tfm/public/baskervillef/BaskervilleF-Regular.tfm"

defaultFontSize :: Word32
defaultFontSize = 65536 * 10

defaultInterlineFactor :: Float
defaultInterlineFactor = 1.5

defaultInterlineSpace :: Int
defaultInterlineSpace = round $ defaultInterlineFactor * fromIntegral defaultFontSize

defaultNumberPosition :: PB.Position 
defaultNumberPosition = Pos Top Ri

defaultPageOffset :: Int
defaultPageOffset = 2

startOnNonFirstPage :: Bool
startOnNonFirstPage = True

defaultLines :: [Int]
defaultLines = [ fromIntegral $ points (fromIntegral $ fst $ computeBoxSize defaultMargins) ]

defaultPageHeights :: [Int]
defaultPageHeights = concat $ replicate 12 [fromIntegral $ points $ fromIntegral $ approximateNumberOfLines 45]
  where
    approximateNumberOfLines :: Int -> Int
    approximateNumberOfLines n = (n * fromIntegral defaultFontSize + (n-1) * defaultInterlineSpace) `div` 65536

defaultMargins :: PB.Margins
defaultMargins = Margins 72 72 64 64

defaultSpaceFactors :: UT.SimpleSpaceFactors
defaultSpaceFactors = SimpleSpaceFactors (1.0/3.0) (1.0/6.0) (1.0/9.0) 0.0

-- ##################################################################################################################

--  #######   the following section contains previous attempts of implementing the page layout algorithm  ######
--  ####### these are kept here to give an idea of how to implement it step by step using the dvi-library ######

defaultLinebreaker :: [LB.Item Char] -> [Int] -> LB.Parameters -> Font -> String -> IO ()
defaultLinebreaker items lines' parameters' font fileName = do
  let
    result = LB.processOutput (Just '-') items $ crashErrorHandling $ LB.knuthPlassLineBreaking items lines' parameters'
    node = simpleDviNodeOutput font defaultInterlineFactor result
  singlePageDocument node $ "./files/" ++ fileName

-- first attempt to make multi-page documents, by building them by hand
multiPageLinebreaker :: [LB.Item Char] -> [Int] -> LB.Parameters -> Font -> String -> IO()
multiPageLinebreaker items lines' parameters' font fileName = do
  let
    -- result = list of lines (line == list of items)
    result = concat $ replicate 7 $ LB.processOutput (Just '-') items $ crashErrorHandling $ LB.knuthPlassLineBreaking items lines' parameters'
    pages = simplePageBreaking (preparePage result (fromIntegral defaultFontSize) defaultInterlineFactor) (snd $ computeBoxSize defaultMargins)
    node1 = simpleDviNodeOutput font defaultInterlineFactor (getLines $ head pages)
    node2 = simpleDviNodeOutput font defaultInterlineFactor (getLines $ pages !! 1)
  print (length $ simplePageBreaking (preparePage result (fromIntegral defaultFontSize) defaultInterlineFactor) (snd $ computeBoxSize defaultMargins))
  createDVI ("./files/" ++ fileName) 1000 dviUnitsTeX
   >>= shipOut . Page (pageNum (1 :: Int))
                      (shiftPage (points (fromIntegral $ PB.getLeftMargin defaultMargins), points (fromIntegral $ PB.getTopMargin defaultMargins)) $ renderNode node1)
   >>= shipOut . Page (pageNum (2 :: Int)) (shiftPage (fromIntegral $ PB.getLeftMargin defaultMargins, -nodeHeight node2) $ renderNode node2)
  >>= finishDVI
  where
    pageContent font ilf margs p = shiftPage (points (fromIntegral $ PB.getLeftMargin margs), points (fromIntegral $ PB.getTopMargin margs)) $
                                                        renderNode (simpleDviNodeOutput font ilf (getLines p))


--  first attempt to create a document using the page-breaking algorithm and the DVI-processing, printing just one page
makePage :: Either PageBreakError [[LB.Item [LB.Item Char]]] -> Font -> IO()
makePage pages font = createDVI ("./files/" ++ "something.dvi") 1000 dviUnitsTeX
                      >>= shipOut . Page (pageNum (1 :: Int))
                        (shiftPage (points (fromIntegral $ PB.getLeftMargin defaultMargins), points (fromIntegral $ PB.getTopMargin defaultMargins)) $ renderNode (PB.dviNodeOutput font (fromRight [] pages) !! 1))
                      >>= finishDVI


-- used for debugging, prints the result of the algorithm
examplePB :: IO()
examplePB = do
    font <- defaultFont
    let
      items = concatMap (\x -> stringToItems font x defaultSpaceFactors False) (TT.mobyDickChapterOneEng ++ [" "] ++ TT.mobyDickChapterTwoEng ++[" "] ++ TT.mobyDickChapterThreeEng ++[" "] ++ TT.mobyDickChapterFourEng)
    print $ prune $ PB.knuthPlassPageBreaking (fromRight [] (LB.knuthPlassLineBreaking' items (Just '-') defaultLines defaultLBParameters)) defaultPageHeights defaultInterlineSpace defaultPBParameters

-- ################################################################################################################

-- ####### the following section contains the functions for the working implementation of the page layout algorithm ######

-- creates a document using the non-knuth-plass, simple pagebreaking algorithm
ownDoc :: String -> Settings -> [String] -> IO ()
ownDoc fileName set@Settings {lbParams=lbPar, font=ft, fontSize=fS, ilFactor=ilF, spaceFactors=sF, lineWidth=lW, margins=margs} txt = do
  font <- ft
  let
    items = concatMap (\x -> stringToItems font x sF False) txt
    items' = LB.processOutput (Just '-') items $ crashErrorHandling $ LB.knuthPlassLineBreaking items lW lbPar
    pages = simplePageBreaking (preparePage items' (fromIntegral fS) ilF) (snd $ computeBoxSize margs)
  multiPageLinebreaker' pages set font fileName


-- creates a document, using the pagebreaking algorithm
kpbDoc :: String -> Settings -> [String] -> IO()
kpbDoc fileName set@Settings {lbParams=lbPar, pbParams=pbPar, font=ft, fontSize=fS, ilFactor=ilF, spaceFactors=sF, lineWidth=lW, pageHeight=pH} txt= do
      font <- ft
      let
        items = concatMap (\x -> stringToItems font x sF False) txt
        pages = prune $ PB.knuthPlassPageBreaking (LB.processOutput Nothing items (UT.crashErrorHandling $ LB.knuthPlassLineBreaking items lW lbPar))
                                                  pH (round $ ilF * fromIntegral fS) pbPar
      multiPageLinebreaker'' pages set font fileName


-- creates multi-page documents out of the result of the simple page-breaking algorithm
multiPageLinebreaker' :: [PB.Page] -> Settings -> Font -> String -> IO()
multiPageLinebreaker' pages set@Settings {ilFactor=ilF, numberPosition=nPos, pageOffset=pOff, startOnOffset=sOnOff, margins=margs, docType = dT} font fileName = do
  createDVI ("./files/" ++ fileName) 1000 dviUnitsTeX
    >>= mPLhelper 1 pages font ilF margs
    >>= finishDVI
  where
    mPLhelper :: Int -> [PB.Page] -> Font -> Float -> PB.Margins -> DocStat -> IO DocStat
    mPLhelper _ [] _ _ _ _= error "no valid document"
    mPLhelper i [p] font iLFactor margs ds = (shipOut . Page (pageNum (i :: Int)) (concat $ pageContent i font iLFactor margs p)) ds
    mPLhelper i (p:ps) font iLFactor margs ds = (shipOut . Page (pageNum (i :: Int)) (concat $ pageContent i font iLFactor margs p) ) ds
                                                        >>= mPLhelper (i + 1) ps font iLFactor margs
    pageContent i font ilf margs p = shiftPage (points (fromIntegral $ PB.getLeftMargin margs), points (fromIntegral $ PB.getTopMargin margs))
                                                        (renderNode (simpleDviNodeOutput font ilf (getLines p))) :
                                                        PB.pageNumbers dT nPos font ilf i pOff sOnOff


--  alternative version of multiPageLinebreaker' for the page-breaking algorithm instead of the simple one
multiPageLinebreaker'' :: Either PageBreakError [[LB.Item [LB.Item Char]]] -> Settings -> Font -> String -> IO()
multiPageLinebreaker'' (Right pages) set@Settings {ilFactor=ilF, numberPosition=nPos, pageOffset=pOff, startOnOffset=sOnOff, pageHeight=pH, margins=margs, docType=dT} font fileName = do
  createDVI ("./files/" ++ fileName) 1000 dviUnitsTeX
  >>= mPLhelper' 1 pages font
  >>= finishDVI
  where
    mPLhelper' :: Int -> [[LB.Item [LB.Item Char]]] -> Font -> DocStat -> IO DocStat
    mPLhelper' _ [] _ _= error "no valid document"
    mPLhelper' i [p] font ds = (shipOut . Page (pageNum (i :: Int)) (concat $ pageContent' i font p)) ds
    mPLhelper' i (p:ps) font ds | i <= length pH = (shipOut . Page (pageNum (i :: Int)) (concat $ pageContent' i font p)) ds
                                          >>= mPLhelper' (i+1) ps font
                                | otherwise = (shipOut . Page (pageNum (i :: Int)) (concat $ pageContent' i font p)) ds
    pageContent' i font p = shiftPage (points (fromIntegral $ PB.getLeftMargin margs), points (fromIntegral $ PB.getTopMargin margs)) (renderNode (PB.dviNodeOutput font pages !! (i-1)))
                             : PB.pageNumbers dT nPos font ilF i pOff sOnOff
multiPageLinebreaker'' _ _ _ _ = error "no valid document"