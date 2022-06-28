module LineVerification (
  weightsToDistributionArray,
  runComparison,
  runMultiples,
  writeErrorFile
) where
  
import Data.Array
import Data.List
import System.Random.Stateful
import Control.Monad
import System.Process
import GHC.IO.Exception
import Graphics.DVI
import Linebreaking
import Utilities
import System.Directory
import System.IO

randomNumbers :: Int -> Int -> Int -> [Int]
randomNumbers seed amount highest =
  let  
    rollsM = replicateM amount . uniformRM (0, highest)
    pureGen = mkStdGen seed
  in
    runStateGen_ pureGen rollsM :: [Int]

weightsToDistributionArray :: [Int] -> Array Int Int
weightsToDistributionArray weights =
  let 
    (_, indexList) = foldl (\(i, ws) w -> (i+1, (i,w):ws)) (0, []) weights
    weightList = concatMap (\(i, w) -> replicate w i) indexList
  in
    listArray (0, length weightList - 1) weightList  
      
allowedCharacters :: Array Int Char
allowedCharacters = listArray (0, 35) "abcdefghijklmnopqrstuvwxyz0123456789"
  
generateString :: Array Int Int -> Int -> Int -> Int -> String
generateString distributionArray wordAmount lengthSeed charSeed =
  let 
    lengthGen = randomNumbers lengthSeed wordAmount (length distributionArray - 1) 
    lengthList = map (distributionArray !) lengthGen
    totalLength = sum lengthList
    charGen = randomNumbers charSeed totalLength (length allowedCharacters - 1)
    packagedChars = map (\n -> '{' : (allowedCharacters ! n : "}")) charGen
  in 
    zipWords lengthList packagedChars
    
zipWords :: [Int] -> [String] -> String    
zipWords (l:ls) (c:cs) | l > 0 =
  c ++ zipWords (l-1 : ls) cs
zipWords (_:ls) (c:cs) =
  ' ' : zipWords ls (c:cs)
zipWords _ _ = ""
  
addZeroKern :: String -> String 
addZeroKern (c:c':cs) =
  if c == '}' && c' /= ' ' then "}\\strut" ++ addZeroKern (c':cs) else c : c' : addZeroKern cs
addZeroKern s = s  
  
writeTexFile :: String -> IO () 
writeTexFile text = do
  contents <- readFile "template.tex"
  let
    text' = addZeroKern text 
    modifiedContents = intercalate "\n" (map (\l -> if l == "placeholder" then text' else l) $ lines contents)  
  unless (null modifiedContents) $ writeFile "texRandomText.tex" modifiedContents
  
callLatex :: IO ExitCode
callLatex = do
  handle <- runCommand "latex texRandomText.tex" 
  waitForProcess handle

checkTexLog :: IO [Float]
checkTexLog = do
  logFile <- readFile "texRandomText.log"
  let
    relevantLines = filter (isPrefixOf $ "....." ++ ['\\'] ++ "hbox(") $ lines logFile
    ratios = map (last . words) relevantLines
    adjustedRatios = map (\w -> read $ if  "l" `isSuffixOf` w then "0" else w) ratios
  print $ last adjustedRatios
  return adjustedRatios

callAlgorithm :: String -> IO [Float]
callAlgorithm text = do
  font <- loadFont (Right 655360) "C:/texlive/2021/texmf-dist/fonts/tfm/public/cm/cmr10.tfm"
  let 
    parameters' = Parameters 10000 0 10000 10000
    lines' = [fromIntegral $ centimetres 8]
    spaceFactors = defaultSpaceFactors { indentSize = 0.0 }
    hyphenate = False
    interlineFactor = 1.2
    adjustedText = filter (\c -> c /= '{' && c /= '}' && c /= '/' && c /= '\\') text
    items = stringToItems font adjustedText spaceFactors hyphenate
    result = crashErrorHandling $ knuthPlassLineBreaking items lines' parameters'
    node = simpleDviNodeOutput font interlineFactor $ processOutput (Just '-') items result
  singlePageDocument node "haskellRandomText.dvi"
  return $ map snd result

  
callDviType :: IO ()
callDviType = do
  texHandle <- runCommand "dvitype texRandomText.dvi > texType" 
  _ <- waitForProcess texHandle
  haskellhandle <- runCommand "dvitype haskellRandomText.dvi > haskellType" 
  _ <- waitForProcess haskellhandle
  return ()
  
readDviType :: String -> IO [String]
readDviType filePath = do
  typeFile <- readFile filePath
  let relevantLines = filter (\s -> head s == '[') $ lines typeFile
  return relevantLines
  
compareLines :: [String] -> [String] -> Int
compareLines doc1 doc2 =
  foldl (\badLines (line1, line2) -> if line1 /= line2 then badLines + 1 else badLines) 0 $ zip doc1 doc2 

simplifiedDemerits :: [Float] -> Float
simplifiedDemerits ratios =
  sum $ map (\r -> (1 + 100 * abs r**3)**2) ratios

compareDocuments :: IO Float 
compareDocuments = do
  texDocument <- readDviType "texType"
  haskellDocument <- readDviType "haskellType"
  let 
    texDocument' = if head texDocument == "[ ]" then tail texDocument else texDocument
    texDocument'' = head texDocument' : map (delete ' ') (tail texDocument')
    (texLength, haskellLength) = (length texDocument'', length haskellDocument)
    maxLength = max texLength haskellLength 
    lengthDeviation = abs (texLength - haskellLength)
    contentDeviation = compareLines texDocument'' haskellDocument
  return $ 1 - (fromIntegral (lengthDeviation + contentDeviation) / fromIntegral maxLength)
  
runComparison :: Array Int Int -> Int -> Int -> Int-> IO (Float, Float, Float)
runComparison distributionArray wordAmount lengthSeed charSeed = do
  let text = generateString distributionArray wordAmount lengthSeed charSeed
  writeTexFile text
  _ <- callLatex
  texRatios <- checkTexLog
  haskellRatios <- callAlgorithm text
  callDviType
  similarity <- compareDocuments
  let
    texDemerits = simplifiedDemerits texRatios
    haskellDemerits = simplifiedDemerits haskellRatios
  return (similarity, texDemerits, haskellDemerits)

  
runMultiples :: String -> Int -> [Int] -> Int -> IO ()
runMultiples fileName n weightList wordAmount = do
  setCurrentDirectory "./files"
  result <- runMultiplesRecursion n (weightsToDistributionArray weightList) wordAmount (0, 0, [])
  writeErrorFile fileName weightList wordAmount result
  setCurrentDirectory ".."
  return ()

runMultiplesRecursion :: Int -> Array Int Int -> Int -> (Int,Int,[(String,Int,Int)]) -> IO (Int,Int,[(String,Int,Int)])
runMultiplesRecursion n distributionArray wordAmount (m, h, seeds) | n > 0 = do
  lengthSeed <- randomIO :: IO Int
  charSeed <- randomIO :: IO Int
  (ratio, texDemerits, haskellDemerits) <- runComparison distributionArray wordAmount lengthSeed charSeed
  let
    haskellBetter = texDemerits > haskellDemerits
    acc =
      if ratio == 1.0 
      then (m, h, ("Equal", lengthSeed, charSeed) : seeds)
      else
        (m+1,
         if haskellBetter 
         then h+1 
         else h,
         if haskellBetter 
         then ("Haskell lower", lengthSeed, charSeed) : seeds 
         else ("TeX lower", lengthSeed, charSeed) : seeds)
  runMultiplesRecursion (n-1) distributionArray wordAmount acc
runMultiplesRecursion _ _ _ m = return m

writeErrorFile :: String -> [Int] -> Int ->  (Int, Int, [(String,Int,Int)]) -> IO ()
writeErrorFile name weightList wordAmount (errors, demerits, seeds) = do
  file <- openFile name WriteMode
  hPrint file $ "distribution: " ++ show weightList
  hPrint file $ "words: " ++ show wordAmount
  hPrint file $ "errors: " ++ show errors
  hPrint file $ "haskell demerits lower: " ++ show demerits
  hPrint file "seeds: "
  writeSeeds file seeds
  hClose file

writeSeeds :: Handle -> [(String, Int, Int)] -> IO ()
writeSeeds _ [] = return ()
writeSeeds file ((description, lengthseed, charseed):rest) = do
  hPrint file $ description ++ " " ++ show lengthseed ++ ", " ++ show charseed
  writeSeeds file rest

