{-# LANGUAGE DeriveGeneric #-}

module Messages
    ( 
        Message (..),
        test_message_class
    ) where

import GHC.Generics
import Data.UUID
import Users

data Message = Message {
    message_id :: UUID,
    message_content :: String,
    time_created :: Int,
    user_sent_to :: User,
    user_sent_from :: User
} deriving (Show, Generic)

test_message_class :: IO ()
test_message_class = putStrLn "Message Class"