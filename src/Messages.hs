module Messages
    ( 
        Message (..),
        test_message_class
    ) where

import Users

data Message = Message {
    message_id :: Int,
    message_content :: String,
    time_created :: String,
    user_sent_to :: User,
    user_sent_from :: User
}

test_message_class :: IO ()
test_message_class = putStrLn "Message Class"