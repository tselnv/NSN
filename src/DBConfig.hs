module DBConfig (connInfo
                , SessionEnv(..)
                , Password
                )  where


import Control.Monad.Reader

import Database.PostgreSQL.Simple
import Data.Word


type Host = String
type Port = Word16
type Username = String
type Password = String
type DBName = String


-- ------------------------------------------------

data SessionEnv = SessionEnv
                { envConnection :: Connection
                , envStatus :: String }



-- --------------------------------------------


connInfo :: DBName -> Host -> Potr -> Username -> Password -> ConnectInfo
connInfo db host port uname pwd = ConnectInfo
            { connectHost = host
            , connectPort = port
            , connectUser = uname
            , connectPassword = pwd
