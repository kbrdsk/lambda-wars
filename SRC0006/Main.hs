{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns                        #-}
{-# LANGUAGE ViewPatterns                                                    #-}

-- http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/syntax-extns.html
-- http://hackage.haskell.org/package/gloss-1.8.1.2/docs/Graphics-Gloss.html

module Main where

import Control.Applicative
import System.IO

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment    (getArgs)

import Graphics.Gloss        
import Graphics.Gloss.Interface.IO.Game                                      {- 1.8.2.1 -}
import System.Random
import Data.List

import Config
import Model
import View
import Controller

-- | Main

{-
Background  16161D
Particles   E8D96B
whiteish    FFFDF9
Cyan        8AFFCD
red         FF6D63
-}

main :: IO ()
main = do
    -- arguments and current time
    args  <- getArgs
    time  <- round <$> getPOSIXTime
    -- high scores
    h     <- openFile "high.score" ReadMode
    hList <- hGetContents h
    let hList' = reverse $ sort $ map (\x -> read x :: Int) $ words hList
    putStrLn "HIGHSCORES: "
    sequence $ map (putStrLn . show) $ zip [1..] hList' 
    hClose h

    let g = mkStdGen time
    let x' = randomRs ((-400), 400) g :: [Float]
    let x'' = let pair = \(y:y':ys) -> (y, y'):(pair ys) in pair x'
    let initial'        = initial time x'' (take 10 $ reverse $ sort $ (replicate 10 0 ++ hList'))
    let (w, h, display) = chooseDisplay args
    let background      = makeColor 0.047 0.043 0.055 1.0 
    --let background      = makeColor 0 0 0 1.0
    let fps             = 60
    playIO display background fps initial' (draw w h) eventHandler (timeHandler w h) 

-- | Choose a display mode. Note that the resolution of a full screen mode
--   should likely match the resolution of your monitor exactly.
chooseDisplay :: [String] -> (Float, Float, Display)
chooseDisplay []
    = ( defaultHorizontalResolution, defaultVerticalResolution
      , InWindow "Lambda Wars"
                 (round defaultHorizontalResolution
                 ,round defaultVerticalResolution  )
                 (100,100)
      )
chooseDisplay [read -> horizontal, read -> vertical]
    = ( fromIntegral horizontal, fromIntegral vertical
      , FullScreen (horizontal, vertical)
      )
