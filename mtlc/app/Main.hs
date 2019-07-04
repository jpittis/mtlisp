module Main where

import Parser.Parser

import qualified Data.Text as Text (pack)
import Control.Monad.Trans
import System.Console.Haskeline
import Text.Megaparsec (parseTest)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    maybeInput <- getInputLine "mtli> "
    case maybeInput of
      Nothing    -> outputStrLn "Salut!"
      Just input -> (liftIO $ process input) >> loop

process :: String -> IO ()
process line =
  parseTest sexpr (Text.pack line)
