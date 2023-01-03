{-# LANGUAGE DeriveGeneric #-}

module Users
    ( 
        User (..),
        test_user_class
    ) where

import GHC.Generics

data User = User
    {
        user_id :: Int,
        username :: String
    } deriving (Show, Generic)

test_user_class :: IO ()
test_user_class = putStrLn "User Class"