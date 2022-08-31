module Main where

import Graphics.DVI as DVI
import System.Directory
import Linebreaking as LB
import Utilities
import System.IO
import Data.Time
import qualified TestText as TT
import qualified Pagebreaking as PB
import qualified PageExamples as PE

main :: IO ()
main = do
-- generates the documents and saves them in the \files location
  print "Moby Dick:"
  ct <- getCurrentTime
  print "   Simple approach start:"
  print ct
  PE.ownDoc "mobyDickSA.dvi" PE.mdSettings (TT.mobyDickChapterOneEng ++ [" "] ++ TT.mobyDickChapterTwoEng ++[" "] ++ TT.mobyDickChapterThreeEng ++[" "] ++ TT.mobyDickChapterFourEng)
  ct <- getCurrentTime
  print "   Knuth-Plass approach start:"
  print ct
  PE.kpbDoc "mobyDickKPA.dvi" PE.mdSettings (TT.mobyDickChapterOneEng ++ [" "] ++ TT.mobyDickChapterTwoEng ++[" "] ++ TT.mobyDickChapterThreeEng ++[" "] ++ TT.mobyDickChapterFourEng)
  ct <- getCurrentTime
  print ct
  print "done"

  print "Metamorphosis:"
  ct <- getCurrentTime
  print "   Simple approach start:"
  print ct
  PE.ownDoc "metamorphosisSA.dvi" PE.meSettings TT.metamorphosisChapterOneEng
  ct <- getCurrentTime
  print "   Knuth-Plass approach start:"
  print ct
  PE.kpbDoc "metamorphosisKPA.dvi" PE.meSettings TT.metamorphosisChapterOneEng
  ct <- getCurrentTime
  print ct
  print "done"

  print "The picture of Dorian Gray:"
  ct <- getCurrentTime
  print "   Simple approach start:"
  print ct
  PE.ownDoc "thePictureOfDorianGraySA.dvi" PE.dgSettings TT.pictureOfDorianGray
  ct <- getCurrentTime
  print "   Knuth-Plass approach start:"
  print ct
  PE.kpbDoc "thePictureOfDorianGrayKPA.dvi" PE.dgSettings TT.pictureOfDorianGray
  ct <- getCurrentTime
  print ct
  print "done"

  return ()