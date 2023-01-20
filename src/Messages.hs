{-# LANGUAGE DeriveGeneric #-}

module Messages
    ( 
        Message (..),
        test_message_class
    ) where

import GHC.Generics
import Data.UUID
import Users

-- | Message datatype
data Message = Message {
    message_id :: UUID,
    message_content :: String,
    time_created :: Int,
    user_sent_to :: User,
    user_sent_from :: User
} deriving (Show, Generic)

-- | Function used to test imports of message class
test_message_class :: IO ()
test_message_class = putStrLn "Message Class"