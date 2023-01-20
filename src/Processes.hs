module Processes (
    user_thread,
    message_thread,
    print_messages,
    count_messages_for_user,
    full_user_list,
    create_user,
    create_message,
    random_val_in_range,
    random_message,
    gen_uuid
) where 

import Control.Concurrent
import System.IO
import Users
import Messages
import System.Random
import Data.UUID
import Data.UnixTime

-- | User thread process
-- Takes boolean of debug mode
-- Takes Channel of Message Type
-- Takes singular user responsible for
-- Outputs Nothing
user_thread :: Bool -> Chan Message -> User -> IO ()
user_thread show_timeouts messages_chan user = do
    putStrLn ("Starting User" ++ (show (user_id user)) ++ " Thread")
    let possible_users = filter (\a -> a/=user) full_user_list
    forkIO $ ( message_thread show_timeouts messages_chan user possible_users)
    forkIO $ ( message_thread show_timeouts messages_chan user possible_users)
    forkIO $ ( message_thread show_timeouts messages_chan user possible_users)
    forkIO $ ( message_thread show_timeouts messages_chan user possible_users)
    forkIO $ ( message_thread show_timeouts messages_chan user possible_users)
    forkIO $ ( message_thread show_timeouts messages_chan user possible_users)
    forkIO $ ( message_thread show_timeouts messages_chan user possible_users)
    forkIO $ ( message_thread show_timeouts messages_chan user possible_users)
    forkIO $ ( message_thread show_timeouts messages_chan user possible_users)
    forkIO $ ( message_thread show_timeouts messages_chan user possible_users)
    return ()


-- | Message Thread - Runs 10 times per user defined
-- Takes channel to denote when all user messages are completed
-- takes channel to store message created
-- takes user to show who sent the message
-- takes a list of remaining users who can be sent the randomised message
message_thread :: Bool -> Chan Message -> User -> [User] -> IO ()
message_thread show_timeouts messages_chan s_user u_list = do
    -- Sleep random time
    delay <- random_val_in_range 10000000

    -- If timeouts are shown then print out random timeout
    if show_timeouts
        then putStrLn ("Writting Message in " ++ (show delay) ++ " microseconds")
    else pure ()

    -- Delay the thread based on the random timeout
    threadDelay delay
    -- Pick random user from list
    user_index <- random_val_in_range ((length u_list) -1)
    let selected_user = u_list !! user_index
    -- Generate string
    message_content_generated <- random_message 100
    -- Generate UUID for the message
    let m_id = gen_uuid delay
    
    -- Get unix timestamp
    time <- getUnixTime >>= return . show . utSeconds
    let time_int = read time

    -- Create Message
    let message_created = create_message m_id message_content_generated time_int selected_user s_user
    writeChan messages_chan message_created

-- | Function used to print out all messages in clean format
print_messages :: Message -> IO ()
print_messages message_test = do
    putStrLn (show message_test)

-- | Function to print out how many messages were sent to select user
-- Takes list of messages
-- Takes User to see how many messages were received by
-- prints number of messages recieved
count_messages_for_user :: [Message] -> User -> IO ()
count_messages_for_user message_list filter_user = do
    let user_messages_revieved = filter (\lambda_message -> (user_sent_to lambda_message) == filter_user) message_list
    let num_messages = length user_messages_revieved
    putStrLn ((username filter_user) ++ " Recieved " ++ (show num_messages) ++ " Messages")

-- | Full list of users in the system. Returns a pre-populated list of users
-- In a real life situation, this data would be populated through a database
full_user_list :: [User]
full_user_list = [
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

-- | Function used to create a new user in simple format
create_user :: Int -> String -> User
create_user id_parse username_parse = User {user_id = id_parse, username = username_parse}

-- | Function used to create a new message in simple format
create_message :: UUID -> String -> Int -> User -> User -> Message
create_message message_id_parse message_content_parse time_created_parse user_sent_to_parse user_sent_from_parse = Message {message_id = message_id_parse, message_content = message_content_parse, time_created = time_created_parse, user_sent_to = user_sent_to_parse, user_sent_from = user_sent_from_parse}

-- Random Functions
-- ---------------------
-- | Calculates a random number between 0 and an inputted number
random_val_in_range :: Int -> IO Int
random_val_in_range max_val = getStdRandom (randomR (0,max_val))

-- | Generates a random string of characters of given length
random_message :: Int -> IO String
random_message string_length = do
    gen <- newStdGen
    let random_message = take string_length $ randomRs ('a','z') gen
    return random_message

-- | UUID Generator modified from https://stackoverflow.com/questions/58906666/haskell-uuid-generation
gen_uuid :: Int -> UUID
gen_uuid parsed_seed = 
    let seed = parsed_seed
        seeded_gen = mkStdGen seed
        (uuid_generated, _) = random seeded_gen
    in uuid_generated