module Main (
    main
) where 

import Processes
import Control.Concurrent
import System.IO

-- | Main
-- Spins
main :: IO ()
main = do
    let show_timeouts = True
    let show_messages = True

    putStrLn ("Starting Message Simulation")

    messages_chan <- newChan
    messages_duped_chan <- dupChan messages_chan
    forkIO $ (user_thread show_timeouts messages_chan (create_user 1 "Person 1"))
    forkIO $ (user_thread show_timeouts messages_chan (create_user 2 "Person 2"))
    forkIO $ (user_thread show_timeouts messages_chan (create_user 3 "Person 3"))
    forkIO $ (user_thread show_timeouts messages_chan (create_user 4 "Person 4"))
    forkIO $ (user_thread show_timeouts messages_chan (create_user 5 "Person 5"))
    forkIO $ (user_thread show_timeouts messages_chan (create_user 6 "Person 6"))
    forkIO $ (user_thread show_timeouts messages_chan (create_user 7 "Person 7"))
    forkIO $ (user_thread show_timeouts messages_chan (create_user 8 "Person 8"))
    forkIO $ (user_thread show_timeouts messages_chan (create_user 9 "Person 9"))
    forkIO $ (user_thread show_timeouts messages_chan (create_user 10 "Person 10"))

    mapM_ (\_ -> readChan messages_duped_chan) [1..100]
    putStrLn "All Messages Sent!"

    infinite_messages <- getChanContents messages_chan
    let messages = take 100 $ infinite_messages
    let num_messages = length messages
    
    if show_messages
        then (mapM_ print_messages messages)
    else pure ()

    mapM_ (count_messages_for_user messages) full_user_list
    
    return ()