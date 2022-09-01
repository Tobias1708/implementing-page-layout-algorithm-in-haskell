module Utilities where

import Data.List (intercalate)
import Data.Maybe
import Graphics.DVI as DVI
import Linebreaking as LB

-- based on the at size or point size of the used font. indent size is how many space widths the indent is equal to
data SimpleSpaceFactors = SimpleSpaceFactors {
  widthFactor :: Float,
  stretchFactor :: Float,
  shrinkFactor :: Float,
  indentSize :: Float
}


stringToItems :: Font -> String -> SimpleSpaceFactors -> Bool -> [Item Char]
stringToItems font paragraph spaceFactors hyphenate =
  let
    bulkItems =
      intercalate
        [spaceItem 
          (fromIntegral (atSize font)) 
          (widthFactor spaceFactors) 
          (stretchFactor spaceFactors) 
          (shrinkFactor spaceFactors)] 
        $ map (wordToItems hyphenate font) $ words paragraph
  in
    startItems (indentSize spaceFactors) (fromIntegral (atSize font)) ++ bulkItems ++ endItems

wordToItems :: Bool -> Font -> String -> [Item Char]
wordToItems hyphenate font word =
  wordToItemsHelper hyphenate 0 (length word) font word

wordToItemsHelper :: Bool -> Int -> Int -> Font -> String -> [Item Char]
wordToItemsHelper _ _ _ _ [] = []
wordToItemsHelper hyphenate pos wordLength font (letter:ls) =
  let
    penaltyItem =
      if hyphenate && pos > 2 && pos < wordLength - 2
      then Just $ Penalty 0 (Just $ fromIntegral $ maybe 0 charWidth $ getFontChar font $ fromEnum '-')
      else Nothing
    boxItem =
      Just $ LB.Box (Just letter) (fromIntegral $ maybe 0 charWidth $ getFontChar font $ fromEnum letter) 
  in
    catMaybes [penaltyItem, boxItem] ++ wordToItemsHelper hyphenate (pos + 1) wordLength font ls

spaceItem :: Float -> Float -> Float -> Float -> Item b
spaceItem atSize' widthFactor' stretchFactor' shrinkFactor' =
  LB.Glue (round $ atSize' * widthFactor') (round $ atSize' * stretchFactor') (round $ atSize' * shrinkFactor')

startItems :: Float -> Float -> [Item b]
startItems atSize' indentSize' =
  [LB.Box Nothing 0,  LB.Glue (round $ atSize' * indentSize') 0 0]

-- todo: infinityvalue not big enough for final glue. causes issues with final node selection
endItems :: [Item b]
endItems =
  [LB.Glue 0 6553600000 0, Penalty (-infinity) Nothing]

toMarginPunctuation :: [Item Char] -> [Item Char]
toMarginPunctuation items = toMarginPunctuation' items 0

toMarginPunctuation' :: [Item Char] -> Int -> [Item Char]
toMarginPunctuation' ((LB.Box c width):rest) _ | c == Just '.' || c == Just ',' =
  LB.Box (Just '.') 0 : toMarginPunctuation' rest width
toMarginPunctuation' ((LB.Glue width stretch shrink):rest) p =
  LB.Glue (width + p) stretch shrink : toMarginPunctuation' rest 0
toMarginPunctuation' ((LB.Penalty size (Just _)):rest) _ =
  LB.Penalty size (Just 0) : toMarginPunctuation' rest 0
toMarginPunctuation' (item:rest) _ =
  item : toMarginPunctuation' rest 0
toMarginPunctuation' [] _ = []

toAuthorLines :: Font -> [Item Char] -> [Item Char] -> [Item Char]
toAuthorLines font quote author =
  quote
  ++
  [ LB.Penalty infinity Nothing,
    LB.Glue 0 6553600000 0,
    LB.Penalty 50 Nothing ]
  ++
    startItems (fromIntegral $ atSize font) 8.0
  ++
  [ LB.Box Nothing 0,
    LB.Penalty infinity Nothing,
    LB.Glue 0 6553600000 0]
    ++ author
    ++ [ LB.Penalty (-infinity) Nothing ]

toRaggedRightMargin :: [Item b] -> [Item b]
toRaggedRightMargin ((LB.Glue width stretch shrink):rest) | stretch < 6553600000 && stretch /= 0 && shrink /= 0  =
  [ LB.Glue 0 (3*width) 0, LB.Penalty 0 Nothing, LB.Glue width (-2*width) 0 ] ++ toRaggedRightMargin rest
toRaggedRightMargin (item:rest) =
  item : toRaggedRightMargin rest
toRaggedRightMargin [] = []

toCenteredText :: Font -> [Item Char] -> [Item Char]
toCenteredText font items = 
  [ LB.Box Nothing 0, spaceItem (fromIntegral $ atSize font) 0.0 1.0 0.0 ] ++ toCenteredText' items 0

toCenteredText' :: [Item Char] -> Int -> [Item Char]
toCenteredText' is lastWidth | is == endItems =
  [ LB.Glue 0 (3*lastWidth) 0, LB.Penalty (-infinity) Nothing ]
toCenteredText' ((LB.Glue width stretch shrink):rest) _ | stretch < 1000000000 && stretch /= 0 && shrink /= 0  =
  [ LB.Glue 0 (3*width) 0,
    LB.Penalty 0 Nothing,
    LB.Glue width (-6 * width) 0,
    LB.Box Nothing 0,
    LB.Penalty infinity Nothing,
    LB.Glue 0 (3*width) 0 ]
  ++ toCenteredText' rest width
toCenteredText' (item:rest) lastWidth =
  item : toCenteredText' rest lastWidth
toCenteredText' [] _ = []

indexNamePart :: [Item Char] -> [Item Char]
indexNamePart [] = []
indexNamePart (item:is) =
  case item of
  LB.Glue width _ _ ->
    [ LB.Penalty infinity Nothing,
      LB.Glue (round $ 1.5 * fromIntegral width) (3*width) 0,
      LB.Penalty 0 Nothing,
      LB.Glue (round $ 0.5 * fromIntegral width) (-3*width) (round $ (1.0/3.0) * fromIntegral width) ]
    ++ indexNamePart is
  _ -> item : indexNamePart is

indexMiddlePart :: Font -> [Item Char]
indexMiddlePart font =
  [ LB.Box Nothing 0,
    LB.Penalty infinity Nothing,
    LB.Glue (round (7.2/18.0 * fromIntegral (atSize font))) 100000 (round (7.2/18.0 * fromIntegral (atSize font))),
    LB.Glue (round $ 2.5 * fromIntegral (atSize font)) 0 0,
    LB.Penalty 0 Nothing,
    LB.Glue (round $ -2.5 * fromIntegral (atSize font)) (round $ fromIntegral $ atSize font) 0,
    LB.Box Nothing 0,
    LB.Penalty infinity Nothing,
    LB.Glue 0 (round $ fromIntegral (atSize font)) 0 ]

indexReferencePart :: [Item Char] -> [Item Char]
indexReferencePart [] = []
indexReferencePart (item:is) =
  case item of
  LB.Glue width _ _ ->
    [ LB.Penalty 999 Nothing,
      LB.Glue width (-3*width) (round $ (1.0/3.0) * fromIntegral width),
      LB.Box Nothing 0,
      LB.Penalty infinity Nothing,
      LB.Glue 0 (3*width) 0 ]
    ++ indexReferencePart is
  _ -> item : indexReferencePart is

simpleDviNodeOutput :: Font -> Float -> [[Item Char]] -> Node
simpleDviNodeOutput font interlineFactor items =
  wrapNode
  $ BoxNode Vertical 0 0 0 (GlueSet 0 Fill Stretching)
  $ interlineGlue 
      0 
      (DVI.Glue (round $ interlineFactor * fromIntegral (atSize font)) (GlueSS 0 0 0 0) (GlueSS 0 0 0 0)) 
      (wrapNode $ PenaltyNode Nothing)
  $ packItems font items

packItems :: Font -> [[Item Char]] -> [Node]
packItems font
  = map
      (wrapNode
         . BoxNode Horizontal 0 0 0 (GlueSet 0 Fill Stretching)
             . itemsToNodes font)
             
itemsToNodes :: Font -> [Item Char] -> [Node]
itemsToNodes _ [] = []
itemsToNodes font (item:is) =
  case item of
    LB.Box (Just c) _ -> typesetSingleWord font [c] : itemsToNodes font is
    LB.Box Nothing _ -> itemsToNodes font is
    LB.Glue width _ _ -> 
      let glue = DVI.Glue (fromIntegral width) (GlueSS 0 0 0 0) (GlueSS 0 0 0 0)
      in wrapNode (GlueNode glue) : itemsToNodes font is
    _ -> itemsToNodes font is
    
crashErrorHandling :: Either LineBreakError [(Int, AdjRatio)] -> [(Int, AdjRatio)]
crashErrorHandling result =
  case result of
    Left errorCode -> 
      case errorCode of
        NoInput -> error "No Input given"
        NoLines -> error "No Lines given"
        NoRemainingActives -> error "Do something drastic"
        NoSolutionLooseness -> error "No feasible breaks found for this amount of lines with given looseness" 
    Right result' -> result'

simpleStringToDviFile :: Font -> Parameters -> [Int] -> SimpleSpaceFactors -> Bool -> Float -> String -> String -> IO ()
simpleStringToDviFile font parameters' lines' spaceFactors hyphenate interlineFactor text filePath =
  let
    items = stringToItems font text spaceFactors hyphenate
    result = LB.processOutput (Just '-') items $ crashErrorHandling $ LB.knuthPlassLineBreaking items lines' parameters'
    node = simpleDviNodeOutput font interlineFactor result
  in
    singlePageDocument node filePath
    