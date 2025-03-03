-- | This module contains functions for colorful printing into terminal.
module Policeman.ColorTerminal (
  Color (..),
  putStrFlush,
  beautyPrint,
  boldCode,
  blueCode,
  bold,
  boldText,
  boldDefault,
  italic,
  redCode,
  reset,
  resetCode,
  setColor,
  successMessage,
  warningMessage,
  errorMessage,
  infoMessage,
  skipMessage,
) where

import Data.Text
import qualified Data.Text.IO as Text
import System.Console.ANSI (
  Color (..),
  ColorIntensity (Vivid),
  ConsoleIntensity (BoldIntensity),
  ConsoleLayer (Foreground),
  SGR (..),
  setSGR,
  setSGRCode,
 )
import System.IO as IO (hFlush, stdout)

-- | Explicit flush ensures prompt messages are in the correct order on all systems.
putStrFlush :: Text -> IO ()
putStrFlush msg = do
  Text.putStr msg
  IO.hFlush stdout

setColor :: Color -> IO ()
setColor color = setSGR [SetColor Foreground Vivid color]

-- | Starts bold printing.
bold :: IO ()
bold = setSGR [SetConsoleIntensity BoldIntensity]

italic :: IO ()
italic = setSGR [SetItalicized True]

-- | Resets all previous settings.
reset :: IO ()
reset = do
  setSGR [Reset]
  IO.hFlush stdout

-- | Takes list of formatting options, prints text using this format options.
beautyPrint :: [IO ()] -> Text -> IO ()
beautyPrint formats msg = do
  sequence_ formats
  Text.putStrLn msg
  reset

boldText :: Text -> IO ()
boldText message = bold >> putStrFlush message >> reset

boldDefault :: Text -> IO ()
boldDefault message = boldText (" [" <> message <> "]")

colorMessage :: Color -> Text -> IO ()
colorMessage color message = do
  setColor color
  Text.putStrLn $ "  " <> message
  reset

errorMessage, warningMessage, successMessage, infoMessage, skipMessage :: Text -> IO ()
errorMessage = colorMessage Red
warningMessage = colorMessage Yellow
successMessage = colorMessage Green
infoMessage = colorMessage Blue
skipMessage = colorMessage Cyan

blueCode, boldCode, redCode, resetCode :: String
redCode = setSGRCode [SetColor Foreground Vivid Red]
blueCode = setSGRCode [SetColor Foreground Vivid Blue]
boldCode = setSGRCode [SetConsoleIntensity BoldIntensity]
resetCode = setSGRCode [Reset]
