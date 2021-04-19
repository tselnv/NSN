module DBConfig (connInfo
                , SessionEnv(..)
                , Password
                )  where


import Control.Monad.Reader

import Database.PostgreSQL.Simple
import Data.Word


-- type Login = String
type Password = String

-- dbConnectReader :: Reader (Login, Password) ConnectInfo
-- dbConnectReader  = reader env 
--   where env :: (Login, Password) -> ConnectInfo
--         env (login, password) = ConnectInfo
--                    { connectHost = "localhost"
--                    , connectPort = 5432 :: Word16
--                    , connectUser = login
--                    , connectPassword = password
--                    , connectDatabase = "nsn" }

-- ------------------------------------------------

data SessionEnv = SessionEnv
                { envConnection :: Connection
                , envStatus :: String }



-- --------------------------------------------


connInfo :: Password -> ConnectInfo
connInfo pwd = ConnectInfo
            { connectHost = "localhost"
            , connectPort = 5432 :: Word16
            , connectUser = "tsel"
            , connectPassword = pwd
            , connectDatabase = "nsn" }
