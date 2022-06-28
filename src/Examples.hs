module Examples where

import Graphics.DVI as DVI
import Linebreaking as LB
import Utilities as UT
import TestText as TT
import qualified Linebreaking as LB

defaultParameters :: LB.Parameters
defaultParameters = Parameters 10000 10000 infinity infinity

defaultLines :: [Int]
defaultLines = [ fromIntegral $ points 425 ]

defaultFont :: IO Font
defaultFont = loadFont (Right 655360) "C:/texlive/2021/texmf-dist/fonts/tfm/public/cm/cmr10.tfm"

defaultInterlineFactor :: Float
defaultInterlineFactor = 1.2

defaultLinebreaker :: [Item Char] -> [Int] -> LB.Parameters -> Font -> String -> IO ()
defaultLinebreaker items lines' parameters' font fileName = do
  let 
    result = LB.processOutput (Just '-') items $ crashErrorHandling $ LB.knuthPlassLineBreaking items lines' parameters'
    node = simpleDviNodeOutput font defaultInterlineFactor result
  singlePageDocument node $ "./files/" ++ fileName


multiPageLinebreaker :: [Item Char] -> [Int] -> LB.Parameters -> Font -> String -> IO()
multiPageLinebreaker items lines' parameters' font fileName = do
  let
    result = LB.processOutput (Just '-') items $ crashErrorHandling $ LB.knuthPlassLineBreaking items lines' parameters'
    node1 = simpleDviNodeOutput font defaultInterlineFactor result
    node2 = simpleDviNodeOutput font defaultInterlineFactor (result ++ result)
  (createDVI ("./files/" ++ fileName) 1000 dviUnitsTeX 
   >>= shipOut . Page (pageNum (1 :: Int))
                      (shiftPage (- inches 1, -nodeHeight node1) $ renderNode node1)) 
   >>= shipOut . Page (pageNum (2 :: Int)) (shiftPage (0, -nodeHeight node2) $ renderNode node2)
  >>= finishDVI


twoParagraphs :: IO ()
twoParagraphs = do
  font <- defaultFont
  let
    items1 = stringToItems font TT.loremIpsum2 defaultSpaceFactors False
    items2 = stringToItems font TT.loremIpsum3 defaultSpaceFactors False
    items' = items1 ++ items2
  defaultLinebreaker items' defaultLines defaultParameters font "twoParagraphs.dvi"

patching :: IO ()
patching = do
  font <- defaultFont
  let
    items = stringToItems font TT.loremIpsum defaultSpaceFactors False
    items' = take (201-17) items ++ [ LB.Penalty (- LB.infinity) Nothing ]
  defaultLinebreaker items' defaultLines defaultParameters font "patching.dvi"
    

marginPunctuation :: IO ()
marginPunctuation = do
  font <- defaultFont
  let 
    items = stringToItems font TT.loremIpsum defaultSpaceFactors True
    items' = toMarginPunctuation items
    result =
      LB.processOutput (Just '-') items
      $ crashErrorHandling
      $ LB.knuthPlassLineBreaking items' defaultLines defaultParameters
    node = simpleDviNodeOutput font defaultInterlineFactor result
  singlePageDocument node "./files/marginPunctuation.dvi"

authorLines :: IO ()
authorLines = do
  font <- defaultFont
  let 
    items = stringToItems font TT.loremIpsum2Debug defaultSpaceFactors False
    itemsSame = drop 2 $ take (length items - 2) items
    itemsNext = drop 2 $ take (length items - 12) items
    author = wordToItems False font "Author" 
    itemsSame' = toAuthorLines font itemsSame author
    itemsNext' = toAuthorLines font itemsNext author
    items' = itemsSame' ++ endItems ++ itemsNext'
  defaultLinebreaker items' defaultLines defaultParameters font "authorLines.dvi"


raggedRight :: IO ()
raggedRight = do
  font <- defaultFont
  let items = toRaggedRightMargin $ stringToItems font TT.loremIpsum defaultSpaceFactors False
  defaultLinebreaker items defaultLines defaultParameters font "raggedRight.dvi"

centeredText :: IO ()
centeredText = do
  font <- defaultFont
  let
    items = toCenteredText font $ drop 2 $ stringToItems font TT.loremIpsum defaultSpaceFactors False
  defaultLinebreaker items [fromIntegral $ centimetres 4] defaultParameters font "centeredText.dvi"

complexIndex :: IO ()
complexIndex = do
  font <- defaultFont
  let
    namePart = drop 2 $ stringToItems font "Some Name Part" defaultSpaceFactors False
    namePart' = indexNamePart $ take (length namePart - 2) namePart
    middlePart = indexMiddlePart font
    referencePart = indexReferencePart $ stringToItems font "Some Reference Part" defaultSpaceFactors False
    items = namePart' ++ middlePart ++ referencePart
    result =
      LB.processOutput Nothing items
      $ crashErrorHandling
      $ LB.knuthPlassLineBreaking items [fromIntegral $ centimetres 3] defaultParameters
    result2 =
      LB.processOutput Nothing items
      $ crashErrorHandling
      $ LB.knuthPlassLineBreaking items [fromIntegral $ centimetres 6] defaultParameters
    result3 =
      LB.processOutput Nothing items
      $ crashErrorHandling
      $ LB.knuthPlassLineBreaking items [fromIntegral $ centimetres 8] defaultParameters
    node = simpleDviNodeOutput font defaultInterlineFactor (result ++ [endItems] ++ result2 ++ [endItems] ++ result3)
  singlePageDocument node "./files/complexIndex.dvi"

------------------------------------------------------------------------------------------------------------------------

kerning :: IO()
kerning = do
  font <- defaultFont
  let 
    items = 
      [ LB.Box (Just 'A') (fromIntegral $ maybe 0 charWidth $ getFontChar font $ fromEnum 'A'),
        LB.Glue (- div (fromIntegral (atSize font)) 4) 0 0,
        LB.Box (Just 'V') (fromIntegral $ maybe 0 charWidth $ getFontChar font $ fromEnum 'V'),
        LB.Glue (fromIntegral (atSize font)) 0 0,
        LB.Box (Just 'A') (fromIntegral $ maybe 0 charWidth $ getFontChar font $ fromEnum 'A'),
        LB.Box (Just 'V') (fromIntegral $ maybe 0 charWidth $ getFontChar font $ fromEnum 'V'),
        LB.Glue 0 1000000000 0, 
        LB.Penalty (-LB.infinity) Nothing ]
  defaultLinebreaker items defaultLines defaultParameters font "kerning.dvi"
  
multiGlue :: IO () 
multiGlue = do
  font <- defaultFont
  let 
    boxItem = LB.Box (Just 'A') (fromIntegral $ maybe 0 charWidth $ getFontChar font $ fromEnum 'A')
    items = 
      [ boxItem,
        spaceItem (fromIntegral $ atSize font) (1.0/3.0) 0 0,
        boxItem,
        spaceItem (fromIntegral $ atSize font) (2.0/3.0) 0 0,
        boxItem,
        LB.Glue 0 1000000000 0, 
        LB.Penalty (-LB.infinity) Nothing,
        boxItem,
        spaceItem (fromIntegral $ atSize font) (1.0/3.0) 0 0,
        boxItem,
        spaceItem (fromIntegral $ atSize font) (1.0/3.0) 0 0,
        spaceItem (fromIntegral $ atSize font) (1.0/3.0) 0 0,
        boxItem,
        LB.Glue 0 1000000000 0, 
        LB.Penalty (-LB.infinity) Nothing ]
  defaultLinebreaker items defaultLines defaultParameters font "multiGlue.dvi"
  
explicitBreaks :: IO ()
explicitBreaks = do
  font <- defaultFont
  let
    items = stringToItems font TT.loremIpsum2 defaultSpaceFactors False
    items' = explicitBreaksHelper items False
    items'' = items ++ items'
  defaultLinebreaker items'' defaultLines defaultParameters font "explicitBreaks.dvi"
  
explicitBreaksHelper :: [Item b] -> Bool -> [Item b]
explicitBreaksHelper ((LB.Glue width stretch shrink):rest) True =
  [ LB.Penalty 0 Nothing, LB.Glue width stretch shrink ] ++ explicitBreaksHelper rest False
explicitBreaksHelper ((LB.Box c width):rest) _ =
  LB.Box c width : explicitBreaksHelper rest True
explicitBreaksHelper (item:rest) _ = 
  item : explicitBreaksHelper rest False
explicitBreaksHelper [] _ = []  

emptyLine :: IO ()
emptyLine = do
  font <- defaultFont
  let
    items1 = stringToItems font TT.loremIpsum2 defaultSpaceFactors False
    items2 = stringToItems font TT.loremIpsum3 defaultSpaceFactors False
    items' = items1 ++ endItems ++ items2
  defaultLinebreaker items' defaultLines defaultParameters font "emptyLine.dvi"
  
flaggedPenalty :: IO ()
flaggedPenalty = do 
  font <- defaultFont
  let
    items = stringToItems font TT.loremIpsum2 defaultSpaceFactors True
  defaultLinebreaker items defaultLines defaultParameters font "flaggedPenalty.dvi"
  
prohibitedBreak :: IO ()
prohibitedBreak = do
  font <- defaultFont
  let 
    items = stringToItems font TT.loremIpsum2 defaultSpaceFactors False
    items' = take 41 items ++ [LB.Penalty infinity Nothing] ++ drop 41 items
  defaultLinebreaker items' defaultLines defaultParameters font "prohibitedBreak.dvi"
  
differentLineWidths :: IO ()
differentLineWidths = do
  font <- defaultFont
  let 
    items = stringToItems font TT.loremIpsum defaultSpaceFactors False
    lines' = map fromIntegral
      [centimetres 4,
       centimetres 4,
       centimetres 4,
       centimetres 6,
       centimetres 6,
       centimetres 6,
       centimetres 8,
       centimetres 8,
       centimetres 8,
       centimetres 3,
       centimetres 3,
       centimetres 3,
       centimetres 5,
       centimetres 5,
       centimetres 5,
       centimetres 9,
       centimetres 9,
       centimetres 9,
       centimetres 5,
       centimetres 7,
       centimetres 5,
       centimetres 7,
       centimetres 5,
       centimetres 7,
       centimetres 5,
       centimetres 7,
       centimetres 5,
       centimetres 7]
  defaultLinebreaker items lines' defaultParameters font "differentLineWidths.dvi"
  

tenParagraphs :: IO ()
tenParagraphs = do
  font <- defaultFont
  let
    items1 = stringToItems font TT.loremIpsum2 defaultSpaceFactors False
    items2 = stringToItems font TT.loremIpsum3 defaultSpaceFactors False
    items' = items1 ++ items2 ++ items1 ++ items2 ++ items1 ++ items1 ++ items2 ++ items1 ++ items2 ++ items1
  defaultLinebreaker items' defaultLines defaultParameters font "tenParagraphs.dvi"