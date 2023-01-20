{-# LANGUAGE DeriveGeneric #-}

module Users
    ( 
        User (..),
        test_user_class
    ) where

import GHC.Generics
import Control.Parallel.Strategies

-- | User data type
data User = User
    {
        user_id :: Int,
        username :: String
    } deriving (Show, Generic, Eq)

-- | Function used to test imports of user class
test_user_class :: IO ()
test_user_class = putStrLn "User Class"