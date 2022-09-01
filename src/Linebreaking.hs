module Linebreaking where
 
import Data.Maybe
import Data.Array
import Control.Monad.State

--type and data definitions

type Width = Int
type Stretch = Int
type Shrink = Int
type Demerits = Float
type AdjRatio = Float

data Item b = Box (Maybe b) Width | Glue Width Stretch Shrink | Penalty Float (Maybe Width) deriving (Eq, Show)

data FitnessClass = TightLine | NormalLine | LooseLine | VeryLooseLine deriving (Eq, Show, Ord, Ix)

type FitnessDemeritsArray = Array FitnessClass (Maybe (Demerits, AdjRatio, BreakNode))
type FitnessDemeritsList = [(FitnessClass, Maybe (Demerits, AdjRatio, BreakNode))]

data LineBreakError = NoInput | NoLines | NoSolutionLooseness | NoRemainingActives deriving Show

data Parameters = Parameters {
  flagDemerits :: Demerits,
  fitnessDemerits :: Demerits,
  threshold :: Float,
  looseness :: Float
}

data GlobalState b = GlobalState {
  currentPosition :: Int,
  activeLine :: Int,
  jZeroLine :: Int,
  wasPreviousBox :: Bool,
  parameters :: Parameters,
  activeNodes :: [BreakNode],
  runningWidth :: Width,
  runningStretch :: Stretch,
  runningShrink :: Shrink,
  fitnessDemeritsArray :: FitnessDemeritsArray,
  remainingItems :: [Item b]
}

data BreakNode = BreakNode {
  position :: Int,
  line :: Int,
  fitnessClass :: FitnessClass,
  totalWidth :: Width,
  totalStretch :: Stretch,
  totalShrink :: Shrink,
  totalMinimumDemerits :: Demerits,
  flagged :: Bool,
  breakAdjustmentRatio :: AdjRatio,
  previous :: Maybe BreakNode
} deriving (Eq, Show)

--interface definitions


--helper functions

itemIsBox :: Item b -> Bool
itemIsBox item =
  case item of
  Box {} -> True
  _ -> False

itemIsGlue :: Item b -> Bool
itemIsGlue item =
  case item of
  Glue {} -> True
  _ -> False

itemIsPenalty :: Item b -> Bool
itemIsPenalty item =
  case item of
  Penalty _ _ -> True
  _ -> False

itemIsForcedPenalty :: Item b -> Bool
itemIsForcedPenalty item =
  case item of
  Penalty size _ | size == -infinity -> True
  _ -> False

itemIsFlaggedPenalty :: Item b -> Bool
itemIsFlaggedPenalty item =
  case item of
  Penalty _ (Just _) -> True
  _ -> False

addWidthStretchShrink :: (Width, Stretch, Shrink) -> Item b -> (Width, Stretch, Shrink)
addWidthStretchShrink (lWidth, lStretch, lShrink) item =
  case item of
  Box _ rWidth -> (lWidth + rWidth, lStretch, lShrink)
  Glue rWidth rStretch rShrink -> (lWidth + rWidth, lStretch + rStretch, lShrink + rShrink)
  Penalty _ (Just rWidth) -> (lWidth + rWidth, lStretch, lShrink)
  _ -> (lWidth, lStretch, lShrink)

infinity :: Float
infinity = read "Infinity"

isFitnessNextTo :: FitnessClass -> FitnessClass -> Bool
isFitnessNextTo lf rf =
  if lf < rf
  then rangeSize (lf, rf) < 3
  else rangeSize (rf, lf) < 3

adjustmentRatioToFitnessClass :: AdjRatio -> FitnessClass
adjustmentRatioToFitnessClass ratio = case ratio of
  r | -1 <= r && r < -0.5 -> TightLine
  r | -0.5 <= r && r < 0.5 -> NormalLine
  r | 0.5 <= r && r < 1 -> LooseLine
  _ -> VeryLooseLine

freshFitnessDemeritsArray :: FitnessDemeritsArray
freshFitnessDemeritsArray = listArray (TightLine, VeryLooseLine) $ replicate 4 Nothing

updateFitnessDemeritsArray :: FitnessDemeritsArray -> FitnessClass -> Demerits -> AdjRatio -> BreakNode -> FitnessDemeritsArray
updateFitnessDemeritsArray dfa fitness demerits ratio node = case dfa ! fitness of
  Just (currentDemerits, _, _) ->
    if demerits < currentDemerits then dfa // [(fitness, Just (demerits, ratio, node))] else dfa
  _ -> dfa // [(fitness, Just (demerits, ratio, node))]

minimumDemerits :: FitnessDemeritsArray -> Demerits
minimumDemerits dfa = minimum $ map (\(d, _, _) -> d) $ mapMaybe snd $ assocs dfa


--the algorithm

knuthPlassLineBreaking' :: [Item b] -> Maybe b -> [Width] -> Parameters -> Either LineBreakError [[Item b]]
knuthPlassLineBreaking' items hyphen lines' parameters' =
  fmap (processOutput hyphen items) (knuthPlassLineBreaking items lines' parameters')

knuthPlassLineBreaking :: [Item b] -> [Width] -> Parameters -> Either LineBreakError [(Int, AdjRatio)]
knuthPlassLineBreaking [] _ _ = Left NoInput
knuthPlassLineBreaking _ [] _ = Left NoLines
knuthPlassLineBreaking (i:is) lineLengths parameters' =
  let
    initialNode = BreakNode {
      position = 0,
      line = 0,
      fitnessClass = NormalLine,
      totalWidth = 0,
      totalStretch = 0,
      totalShrink = 0,
      totalMinimumDemerits = 0,
      flagged = False,
      breakAdjustmentRatio = 0,
      previous = Nothing
    }
    initialState = GlobalState {
      currentPosition = 0,
      activeLine = 0,
      jZeroLine =
        fst
        $ foldl (\(j, anchor) (k, next) -> if next == anchor then (j, anchor) else (k, next)) (0, 0)
        $ zip [0..] lineLengths,
      wasPreviousBox = False,
      parameters = parameters',
      activeNodes = [initialNode],
      runningWidth = 0,
      runningStretch = 0,
      runningShrink = 0,
      fitnessDemeritsArray = freshFitnessDemeritsArray,
      remainingItems = i:is
    }
  in
    case runState (traverseParagraph (i:is) lineLengths) initialState of
      (bestBreaks, _) -> bestBreaks


traverseParagraph :: [Item b] -> [Width] -> State (GlobalState b) (Either LineBreakError [(Int, AdjRatio)])
traverseParagraph [] lineLengths = do
  (activeNodes', looseness') <- gets $ \s -> (activeNodes s, looseness $ parameters s)
  return $ getBestBreaks activeNodes' (length lineLengths, looseness')

traverseParagraph (i:is) lineLengths = do
  let 
    mainLoop = do
      (activeNodes', jZeroLine') <- gets $ \s -> (activeNodes s, jZeroLine s)
      activeNodes'' <- traverseNodes activeNodes' i (line $ head activeNodes') jZeroLine' lineLengths      
      modify $ \s -> s
        { activeLine = line $ head activeNodes'', --not needed anymore
          activeNodes = activeNodes'' } 
  case i of
    Box _ width -> do
      modify $ \s -> s { runningWidth = runningWidth s + width, wasPreviousBox = True }
    Glue width stretch shrink -> do
      wasPreviousBox' <- gets wasPreviousBox
      when wasPreviousBox' mainLoop
      modify $ \s ->  s
        { runningWidth = runningWidth s + width,
          runningStretch = runningStretch s + stretch,
          runningShrink = runningShrink s + shrink,
          wasPreviousBox = False }
    Penalty size _ -> do
      modify $ \s -> s { wasPreviousBox = False }
      unless (size == infinity) mainLoop
  activeNodes' <- gets activeNodes
  modify $ \s -> s { currentPosition = 1 + currentPosition s, remainingItems = is }
  if null activeNodes'
  then
    return $ Left NoRemainingActives
  else
    traverseParagraph is lineLengths


traverseNodes :: [BreakNode] -> Item b -> Int -> Int -> [Width] -> State (GlobalState b) [BreakNode]
traverseNodes (node:ns) currentItem currentLine jzero lineLengths | line node <= currentLine || currentLine > jzero = do
  (runningWidth', runningStretch', runningShrink', threshold') <- gets $ \s ->
    (runningWidth s, runningStretch s, runningShrink s, threshold $ parameters s)
  let
    adjustmentRatio = calculateAdjustmentRatio node currentItem lineLengths runningWidth' runningStretch' runningShrink'
    (penaltySize, penaltyFlag) =
      case currentItem of
        Penalty size flag -> (size, isJust flag)
        _ -> (0, False)
    keepNode =
      if adjustmentRatio < -1.0 || penaltySize == -infinity
      then Nothing
      else Just node
  when
    (-1.0 <= adjustmentRatio && adjustmentRatio <= threshold')
    (do
      fitnessDemerits' <- gets (fitnessDemerits . parameters)
      flagDemerits' <- gets (flagDemerits . parameters)
      let (fitness, demerits) =
            calculateDemerits node penaltySize penaltyFlag adjustmentRatio flagDemerits' fitnessDemerits'
      modify $ \s -> s
        { fitnessDemeritsArray =
            updateFitnessDemeritsArray (fitnessDemeritsArray s) fitness demerits adjustmentRatio node })
  rest <- traverseNodes ns currentItem currentLine jzero lineLengths
  case keepNode of
    Just node' -> return $ node' : rest
    Nothing -> return rest

traverseNodes ns currentItem currentLine jzero lineLengths = do
  (demeritFitnessArray', fitnessDemerits', currentPosition') <- gets $ \s ->
      (fitnessDemeritsArray s, fitnessDemerits $ parameters s, currentPosition s)
  let
    (glueWidth, glueStretch, glueShrink, penaltyFlag) =
      case currentItem of
        Box _ _ -> (0,0,0,False)
        Glue width stretch shrink -> (width, stretch, shrink, False)
        Penalty _ flag -> (0,0,0, isJust flag)
  (totalWidth', totalStretch', totalShrink') <- gets $ \s ->
    sumGlues
    (runningWidth s + glueWidth, runningStretch s + glueStretch, runningShrink s + glueShrink)
    (if wasPreviousBox s then [] else remainingItems s)

  let
    newNodes =
      createNodes (assocs demeritFitnessArray') fitnessDemerits' (minimumDemerits demeritFitnessArray') currentPosition' penaltyFlag totalWidth' totalStretch' totalShrink'
  modify $ \s -> s { fitnessDemeritsArray = freshFitnessDemeritsArray }
  if null ns
  then return newNodes
  else do
    rest <- traverseNodes ns currentItem (currentLine + 1) jzero lineLengths
    return $ newNodes ++ rest

sumGlues :: (Width, Stretch, Shrink) -> [Item b] -> (Width, Stretch, Shrink)
sumGlues widthStretchShrink items =
  foldl
    addWidthStretchShrink widthStretchShrink
    (filter itemIsGlue $ takeWhile (\i -> not (itemIsBox i) && itemIsGlue i || (itemIsPenalty i && not (itemIsForcedPenalty i))) items)

--todo: lines
createNodes :: FitnessDemeritsList -> Demerits -> Demerits -> Int -> Bool -> Width -> Stretch -> Shrink -> [BreakNode]
createNodes [] _ _ _ _ _ _ _= []
createNodes ((_, Nothing):rest) fitnessDemerits' minimumDemerits' position' flag totalWidth' totalStretch' totalShrink' =
  createNodes rest fitnessDemerits' minimumDemerits' position' flag totalWidth' totalStretch' totalShrink'
createNodes ((fitness, Just (demerits, ratio, node)):rest) fitnessDemerits' minimumDemerits' position' flag totalWidth' totalStretch' totalShrink' =
  if demerits <= minimumDemerits' + fitnessDemerits'
  then
    let newNode = BreakNode position' (1 + line node) fitness totalWidth' totalStretch' totalShrink' demerits flag ratio (Just node) in
    newNode : createNodes rest fitnessDemerits' minimumDemerits' position' flag totalWidth' totalStretch' totalShrink'
  else createNodes rest fitnessDemerits' minimumDemerits' position' flag totalWidth' totalStretch' totalShrink'


calculateAdjustmentRatio :: BreakNode -> Item b -> [Width] -> Width -> Stretch -> Shrink -> AdjRatio
calculateAdjustmentRatio activeNode currentItem lineLengths runningWidth' runningStretch' runningShrink' =
  let
    penaltyWidth =
      case currentItem of
        Penalty _ (Just flagWidth) -> flagWidth
        _ -> 0
    currentLineWidth' =
      runningWidth' - totalWidth activeNode + penaltyWidth
    desiredLineWidth' =
      if length lineLengths > line activeNode
      then lineLengths !! line activeNode
      else lineLengths !! (length lineLengths - 1)
    stretchShrink' =
      if currentLineWidth' < desiredLineWidth'
      then runningStretch' - totalStretch activeNode
      else runningShrink' - totalShrink activeNode
  in
    if currentLineWidth' == desiredLineWidth'
    then 0
    else
      if stretchShrink' <= 0
      then infinity
      else fromIntegral (desiredLineWidth' - currentLineWidth') / fromIntegral stretchShrink'


calculateDemerits :: BreakNode -> Float -> Bool -> AdjRatio -> Demerits -> Demerits -> (FitnessClass, Demerits)
calculateDemerits activeNode penaltySize' penaltyFlag' adjustmentRatio flagDemerits' fitnessDemerits' =
  let
    totalMinimumDemerits' = totalMinimumDemerits activeNode
    badness' = 1 + 100 * abs (adjustmentRatio**3)
    demeritBase
      | penaltySize' >= 0 = (badness' + penaltySize')**2
      | penaltySize' /= -infinity = badness'**2 - penaltySize'**2
      | otherwise = badness'**2
    newFitnessClass = adjustmentRatioToFitnessClass adjustmentRatio
  in
    (newFitnessClass,
     totalMinimumDemerits'
     + demeritBase
     + if isFitnessNextTo (fitnessClass activeNode) newFitnessClass then 0 else fitnessDemerits'
     + if flagged activeNode && penaltyFlag' then flagDemerits' else 0)


getBestBreaks :: [BreakNode] -> (Int, Float) -> Either LineBreakError [(Int, AdjRatio)]
getBestBreaks nodes (lineNumber, looseness') =
  let
    acceptableNodes =
      filter
        (\node ->
          if looseness' == infinity
          then line node > lineNumber
          else 
            if looseness' == -infinity
            then line node < lineNumber
            else
              inRange (lineNumber, lineNumber + truncate looseness') (line node)
              || inRange (lineNumber + truncate looseness', lineNumber) (line node))
        nodes
    getBestFolder = \acc element -> if totalMinimumDemerits acc < totalMinimumDemerits element then acc else element
    traverseBreaks breakList node = case previous node of
      Just previousNode -> traverseBreaks ((position node, breakAdjustmentRatio node) : breakList) previousNode
      Nothing -> Right breakList
  in
  case acceptableNodes of
    [] -> Left NoSolutionLooseness
    (node:ns) ->
      traverseBreaks [] (foldl getBestFolder node ns)

processOutput :: Maybe b -> [Item b] -> [(Int, AdjRatio)] -> [[Item b]]
processOutput hyphen items optimal =
  map (dropWhile (not . itemIsBox)) $ processOutputHelper hyphen items optimal 0 []

processOutputHelper :: Maybe b -> [Item b] -> [(Int, AdjRatio)] -> Int -> [Item b] -> [[Item b]]
processOutputHelper _ [] _ _ _ = []
processOutputHelper _ _ [] _ _ = []
processOutputHelper hyphen (item:items) optimal@((position', ratio):rest) posAcc lineAcc =
  if position' == posAcc
  then
    let
      lineAcc' =
        if itemIsFlaggedPenalty item && isJust hyphen
        then lineAcc ++ [Box hyphen 0]
        else lineAcc
    in lineAcc' : processOutputHelper hyphen (item:items) rest posAcc []
  else
    case item of
    Box content width ->
      processOutputHelper hyphen items optimal (posAcc+1) (lineAcc ++ [Box content width])
    Glue width stretch shrink ->
      let
        newWidth =
          width + if ratio >= 0 then round (ratio * fromIntegral stretch) else round (ratio * fromIntegral shrink)
      in processOutputHelper hyphen items optimal (posAcc+1) (lineAcc ++ [Glue newWidth 0 0])
    _ -> processOutputHelper hyphen items optimal (posAcc+1) lineAcc