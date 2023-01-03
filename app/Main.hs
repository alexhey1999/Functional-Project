module Main (main) where

import Users
import Messages

main :: IO ()
main = do
    test_user_class
    test_message_class

create_user :: Int -> String -> User
create_user id username = User {user_id = id, username = username}

create_user_list :: [User]
create_user_list = [
        create_user 1 "Person 1",
        create_user 2 "Person 2",
        create_user 3 "Person 3",
        create_user 4 "Person 4",
        create_user 5 "Person 5",
        create_user 6 "Person 6",
        create_user 7 "Person 7",
        create_user 8 "Person 8",
        create_user 9 "Person 9",
        create_user 10 "Person 10"
    ]

create_thread :: User -> IO
create_thread user = do
    