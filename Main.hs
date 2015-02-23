import Lift

main = do
    putStrLn "What lift?"
    -- lift <- getLine
    let lift = "squat"
    putStrLn "How many sets?"
    -- sets <- getLine
    let sets = "5"
    putStrLn "Input the weight"
    -- weight <- getLine
    let weight = "255"
    putStrLn "Input the reps"
    --reps <- getLine
    let reps = "5"
    let set  = Set (fromInteger (read weight)) (read reps)
        sesh = Session lift $ take (read sets) $ repeat set
    putStr "Your session:\t"
    putStrLn (show sesh)
    let vol = sessionVolume sesh
    putStr "Volume:\t"
    putStrLn (show vol)
