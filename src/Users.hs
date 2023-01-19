{-# LANGUAGE DeriveGeneric #-}

module Users
    ( 
        User (..),
        test_user_class
    ) where

import GHC.Generics
import Control.Parallel.Strategies

data User = User
    {
        user_id :: Int,
        username :: String
    } deriving (Show, Generic, Eq)

test_user_class :: IO ()
test_user_class = putStrLn "User Class"