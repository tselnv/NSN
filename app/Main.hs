module Main where

import Data.Int
import Control.Exception
import Database.PostgreSQL.Simple
import System.Environment (getArgs)

import NSNDB
import NSNTypes
import EE

handleExc :: SomeException -> IO (Either String Int64)
handleExc e = return $ Left $ show e


run :: [String] -> IO ()
run (pwd:[]) = do
  result <- catch (runDB pwd) handleExc
  case result of
    Left e -> print $ "ERROR:  " ++ show e
    Right n -> print $ "done: added rows = " ++ show n
run _ = putStrLn "Usage: nsn password"


main :: IO ()
main = do
  args <- getArgs
  run args
