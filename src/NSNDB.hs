{-# LANGUAGE OverloadedStrings #-}

module NSNDB where


import Control.Monad.Reader
import Data.Int
import Database.PostgreSQL.Simple
import Control.Exception (catch, SomeException)

import DBConfig
import NSNTypes


type Session = Reader SessionEnv



prepareDB :: ConnectInfo -> IO (Either String Connection)
prepareDB connInfo =
  -- add prepare routine here, e.g.
  catch (Right <$> connect connInfo) handleError

  where handleError :: SomeException -> IO (Either String Connection)
        handleError e = return $ Left $ show e

-- ----------------------------------------- LOAD INPUT DATA ------------------------------------

loadInputData :: ReaderT SessionEnv IO (Either String InputData')
loadInputData = do
  -- add load routine here, e.g. 
  dbh <- asks envConnection
  liftIO $ catch (readQuerry dbh) handleError


  where readQuerry :: Connection ->  IO (Either String [(Int, String, String)])
        readQuerry dbh = Right <$> query dbh "SELECT podcast_id, podcast_url, podcast_name FROM podcasts WHERE podcast_id <> (?)" (Only (666 :: Int))

        handleError :: SomeException -> IO (Either String InputData')
        handleError e = return $ Left $ show e



-- ----------------------------------------- WRITE OUTPUT DATA ------------------------------------

writeOutputData :: OutputData' -> ReaderT SessionEnv IO (Either String Int64)
writeOutputData [] = return $ Right 0
writeOutputData ((uid,link,name):_) = do
  -- add write routine here, e.g.
  dbh <- asks envConnection
  liftIO $ catch (writeQuery dbh) handleError

  where writeQuery :: Connection -> IO (Either String Int64)
        writeQuery dbh = Right <$> execute dbh "INSERT INTO podcasts (podcast_id, podcast_url, podcast_name) VALUES (?,?,?)"
                                   (uid,link,name)

        handleError :: SomeException -> IO (Either String Int64)
        handleError e = return $ Left $ show e



-- ----------------------------------------- BUSSINES LOGIC -------------------------

businessLogic :: InputData' -> OutputData'
businessLogic [] = []
businessLogic ((uid, link, name):_) = [(uid + 21, link ++ show (uid + 21), name ++ " ololo!")]

-- ---------------------------------- RUN DB PROCESSING--------------------------------------------

runDB :: Password -> IO (Either String Int64)
runDB pwd = do
  eitherConn <- prepareDB $ connInfo pwd
  case eitherConn of
    Left e -> return $ Left e
    Right conn -> do 
      let snEnv = SessionEnv { envConnection = conn
                             , envStatus = "Test status" }
      inputData <- runReaderT loadInputData snEnv
      case inputData of
        Left e -> return $ Left e
        Right inDt -> do
          print $ businessLogic inDt
          runReaderT (writeOutputData $ businessLogic inDt) snEnv
          --return $ Right ()



-- -------------------------------------------------------------------------------------------------
