import Data.Array



main :: IO()
main = do
    let board = [4,4,4,4,4,4,0,4,4,4,4,4,4,0]
    let user2_part = take 6 board
    let user1_part = take 6 (drop 7 board)

    putStrLn "        GAME MANGALA"
    
    putStrLn ("  | " ++ board_printer user2_part)
    putStrLn ("  | " ++ board_printer user1_part)
    putStrLn "Which player starts first ? (Enter 1 or 2) "
    input <- getLine
    let user_choice = read input :: Int
    game_engine board user_choice

    
    --input <- getLine
    --let user_choice = read input :: Int

board_printer :: [Int] -> String
board_printer [] = ""
board_printer (x:xs) = show x ++ " | " ++ board_printer(xs)

stone_alloc :: [Int] -> Int -> Int -> Int -> Int -> [Int]
stone_alloc board turn_side number hole stop = if number == stop
    then []
    else [(board !! ((hole - 1 + number) `rem` 14)) + 1] ++ (stone_alloc board turn_side (number + 1) hole stop)
game_engine :: [Int] -> Int -> IO()
game_engine [] _ = putStrLn "Game Finished"
game_engine board turn_side = if turn_side == 1
    then do 
        putStrLn "User 1 Enter hole number: "
        input <- getLine
        let hole_choice = read input :: Int

        if board !! (hole_choice - 1) == 1
            then do
                let stone_count = (board !! (hole_choice - 1)) - 1
                let final_board = (take (hole_choice - 1) board) ++ [0] ++ [(board !! hole_choice) + 1] ++ (drop (hole_choice + 1) board)
                if (((hole_choice - 1 + stone_count) `rem` 14) < 5) && ((final_board !! ((hole_choice - 1 + stone_count) `rem` 14)) == 1 ) && (((final_board !! ((hole_choice - 1 + stone_count) `rem` 14)) == 1) || ((stone_count == 0) && ((final_board !! ((hole_choice + stone_count) `rem` 14)) == 1)))
                    then do 
                        let final_board1 = (take hole_choice final_board) ++ [0] ++ (take (5 - hole_choice) (drop (hole_choice + 1) final_board)) ++ [1 + (board !! (12 - hole_choice)) + (board !! 6)] ++ (take (5 - hole_choice) (drop 7 final_board)) ++ [0] ++ (take (hole_choice + 1) (drop (12 - hole_choice) final_board ) )
                        let which_user = 2
                        let user1_part = take 6 final_board1
                        let user2_part = take 6 (drop 7 final_board1)
                        let user1_point = final_board1 !! 6
                        let user2_point = final_board1 !! 13
                                
                        putStrLn "        GAME MANGALA"

                        if user2_point == 0
                            then do
                                putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                if user1_point == 0
                                    then do
                                        putStrLn ("  | " ++ board_printer user1_part)
                                        game_engine final_board1 which_user
                                    else do
                                        putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                        game_engine final_board1 which_user
                            else do
                                putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                if user1_point == 0
                                    then do
                                        putStrLn ("  | " ++ board_printer user1_part)
                                        game_engine final_board1 which_user
                                    else do
                                        putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                        game_engine final_board1 which_user
                else if (((hole_choice - 1 + stone_count) `rem` 14) == 6) || ((stone_count == 0) && (hole_choice == 6))
                    then do
                        let which_user = 1
                        let user1_part = take 6 final_board
                        let user2_part = take 6 (drop 7 final_board)
                        let user1_point = final_board !! 6
                        let user2_point = final_board !! 13
                                
                        putStrLn "        GAME MANGALA"

                        if user2_point == 0
                            then do
                                putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                if user1_point == 0
                                    then do
                                        putStrLn ("  | " ++ board_printer user1_part)
                                        game_engine final_board which_user
                                    else do
                                        putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                        game_engine final_board which_user
                            else do
                                putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                if user1_point == 0
                                    then do
                                        putStrLn ("  | " ++ board_printer user1_part)
                                        game_engine final_board which_user
                                    else do
                                        putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                        game_engine final_board which_user
                    else do
                        let which_user = 2
                        let user1_part = take 6 final_board
                        let user2_part = take 6 (drop 7 final_board)
                        let user1_point = final_board !! 6
                        let user2_point = final_board !! 13
                                
                        putStrLn "        GAME MANGALA"

                        if user2_point == 0
                            then do
                                putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                if user1_point == 0
                                    then do
                                        putStrLn ("  | " ++ board_printer user1_part)
                                        game_engine final_board which_user
                                    else do
                                        putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                        game_engine final_board which_user
                            else do
                                putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                if user1_point == 0
                                    then do
                                        putStrLn ("  | " ++ board_printer user1_part)
                                        game_engine final_board which_user
                                    else do
                                        putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                        game_engine final_board which_user
            else do
                let stone_count = (board !! (hole_choice - 1)) - 1
                
                if (hole_choice + (board !! (hole_choice - 1) - 1)) > 14
                    then do
                        let final_board1 = ((take (hole_choice - 1) board)) ++ [1] ++ (stone_alloc board turn_side 1  hole_choice ((board !! (hole_choice - 1) ))) 
                            
                        let final_board = take 14 ((reverse (take (length final_board1 `rem` 14) (reverse final_board1))) ++ (drop (length final_board1 `rem` 14) final_board1))

                        if (((hole_choice - 1 + stone_count) `rem` 14) < 5) && ((final_board !! ((hole_choice - 1 + stone_count) `rem` 14)) == 1) && (((final_board !! ((hole_choice - 1 + stone_count) `rem` 14)) == 1) || ((stone_count == 0) && ((final_board !! ((hole_choice + stone_count) `rem` 14)) == 1)))
                            then do 
                                let final_board2 = (take ((hole_choice - 1 + stone_count) `rem` 14) final_board) ++ [0] ++ (take (5 - ((hole_choice - 1 + stone_count) `rem` 14)) (drop (((hole_choice - 1 + stone_count) `rem` 14) + 1) final_board)) ++ [1 + (board !! (12 - ((hole_choice - 1 + stone_count) `rem` 14))) + (board !! 6)] ++ (take (5 - ((hole_choice - 1 + stone_count) `rem` 14)) (drop 7 final_board)) ++ [0] ++ (take (((hole_choice - 1 + stone_count) `rem` 14) + 1) (drop (12 - ((hole_choice - 1 + stone_count) `rem` 14)) final_board ) )
                                let which_user = 2
                                let user1_part = take 6 final_board2
                                let user2_part = take 6 (drop 7 final_board2)
                                let user1_point = final_board2 !! 6
                                let user2_point = final_board2 !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board2 which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board2 which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board2 which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board2 which_user
                        else if (((hole_choice - 1 + stone_count) `rem` 14) == 6) || ((stone_count == 0) && (hole_choice == 6))
                            then do
                                let which_user = 1
                                let user1_part = take 6 final_board
                                let user2_part = take 6 (drop 7 final_board)
                                let user1_point = final_board !! 6
                                let user2_point = final_board !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                            else do
                                let which_user = 2
                                let user1_part = take 6 final_board
                                let user2_part = take 6 (drop 7 final_board)
                                let user1_point = final_board !! 6
                                let user2_point = final_board !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                    else do
        
                        let stone_count = (board !! (hole_choice - 1)) - 1
                        let final_board = ((take (hole_choice - 1) board)) ++ [1] ++ (stone_alloc board turn_side 1  hole_choice ((board !! (hole_choice - 1) ))) ++ (drop (hole_choice + (board !! (hole_choice - 1) - 1)) board)
                        
                        if (((hole_choice - 1 + stone_count) `rem` 14) < 5) && ((final_board !! ((hole_choice - 1 + stone_count) `rem` 14)) == 1) && (((final_board !! ((hole_choice - 1 + stone_count) `rem` 14)) == 1) || ((stone_count == 0) && ((final_board !! ((hole_choice + stone_count) `rem` 14)) == 1)))
                            then do 
                                
                                let final_board1 = (take ((hole_choice - 1 + stone_count) `rem` 14) final_board) ++ [0] ++ (take (5 - ((hole_choice - 1 + stone_count) `rem` 14)) (drop (((hole_choice - 1 + stone_count) `rem` 14) + 1) final_board)) ++ [1 + (board !! (12 - ((hole_choice - 1 + stone_count) `rem` 14))) + (board !! 6)] ++ (take (5 - ((hole_choice - 1 + stone_count) `rem` 14)) (drop 7 final_board)) ++ [0] ++ (take (((hole_choice - 1 + stone_count) `rem` 14) + 1) (drop (12 - ((hole_choice - 1 + stone_count) `rem` 14)) final_board ) )
                                let which_user = 2 
                                let user1_part = take 6 final_board1
                                let user2_part = take 6 (drop 7 final_board1)
                                let user1_point = final_board1 !! 6
                                let user2_point = final_board1 !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board1 which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board1 which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board1 which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board1 which_user
                        else if (((hole_choice - 1 + stone_count) `rem` 14) == 6) || ((stone_count == 0) && (hole_choice == 6))
                            then do
                                
                                let which_user = 1  
                                let user1_part = take 6 final_board
                                let user2_part = take 6 (drop 7 final_board)
                                let user1_point = final_board !! 6
                                let user2_point = final_board !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                            else do
                                
                                let which_user = 2
                                let user1_part = take 6 final_board
                                let user2_part = take 6 (drop 7 final_board)
                                let user1_point = final_board !! 6
                                let user2_point = final_board !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                

        else do 
            putStrLn "User 2 Enter hole number:"
            input <- getLine
            let hole_choice = (read input :: Int) + 7
    
            if (board !! (hole_choice - 1)) == 1
                then do
                    let stone_count = (board !! (hole_choice - 1)) - 1
                    let final_board = (take (hole_choice - 1) board) ++ [0] ++ [(board !! hole_choice) + 1] ++ (drop (hole_choice + 1) board)
                    if (((hole_choice - 1 + stone_count) `rem` 14) > 6) && (((hole_choice - 1 + stone_count) `rem` 14) < 12) && (((final_board !! ((hole_choice - 1 + stone_count) `rem` 14)) == 1) || ((stone_count == 0) && ((final_board !! ((hole_choice + stone_count) `rem` 14)) == 1)))
                        then do 
                            putStrLn "a"
                            --let final_board = (take ((hole_choice - 1 + stone_count) `rem` 14) final_board) ++ [0] ++ (take (5 - ((hole_choice - 1 + stone_count) `rem` 14)) (drop ((hole_choice + stone_count) `rem` 14) final_board)) ++ [(final_board !! 6) + (1 + (final_board !! (13 - ((hole_choice - 1 + stone_count) `rem` 14))))] ++ (drop (14 - ((hole_choice - 1 + stone_count) `rem` 14)) final_board)
                            let final_board1 = (take (11 - (hole_choice - 1)) final_board) ++ [0] ++ (take (hole_choice - (13 - hole_choice)) (drop (13 - hole_choice) final_board)) ++ [0] ++ (take (12 - hole_choice) (drop (hole_choice + 1) final_board)) ++ [1 + (board !! (12 - (hole_choice))) + (board !! 13)]
                            --print final_board1
                            let which_user = 1
                            let user1_part = take 6 final_board1
                            let user2_part = take 6 (drop 7 final_board1)
                            let user1_point = final_board1 !! 6
                            let user2_point = final_board1 !! 13
                                
                            putStrLn "        GAME MANGALA"

                            if user2_point == 0
                                then do
                                    putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                    if user1_point == 0
                                        then do
                                            putStrLn ("  | " ++ board_printer user1_part)
                                            game_engine final_board1 which_user
                                        else do
                                            putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                            game_engine final_board1 which_user
                                else do
                                    putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                    if user1_point == 0
                                        then do
                                            putStrLn ("  | " ++ board_printer user1_part)
                                            game_engine final_board1 which_user
                                        else do
                                            putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                            game_engine final_board1 which_user
                        else if (((hole_choice - 1 + stone_count) `rem` 14) == 13) || ((stone_count == 0) && (hole_choice == 13))
                            then do
                                let which_user = 2
                                let user1_part = take 6 final_board
                                let user2_part = take 6 (drop 7 final_board)
                                let user1_point = final_board !! 6
                                let user2_point = final_board !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                            else do
                                let which_user = 1
                                let user1_part = take 6 final_board
                                let user2_part = take 6 (drop 7 final_board)
                                let user1_point = final_board !! 6
                                let user2_point = final_board !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                else do
                    let stone_count = (board !! (hole_choice - 1)) - 1
                    
                    if (hole_choice + (board !! (hole_choice - 1) - 1)) > 14
                        then do
                            
                            let final_board1 = ((take (hole_choice - 1) board)) ++ [1] ++ (stone_alloc board turn_side 1  hole_choice ((board !! (hole_choice - 1) ))) 
                            
                            let final_board = take 14 ((reverse (take (length final_board1 `rem` 14) (reverse final_board1))) ++ (drop (length final_board1 `rem` 14) final_board1))
                            if (((hole_choice - 1 + stone_count) `rem` 14) > 6) && (((hole_choice - 1 + stone_count) `rem` 14) < 12) && (((final_board !! ((hole_choice - 1 + stone_count) `rem` 14)) == 1) || ((stone_count == 0) && ((final_board !! ((hole_choice + stone_count) `rem` 14)) == 1)))
                                then do 
                                    let final_board2 = (take (11 - ((hole_choice - 1 + stone_count) `rem` 14 )) final_board) ++ [0] ++ (take (((hole_choice + stone_count) `rem` 14) - (13 - ((hole_choice + stone_count) `rem` 14))) (drop (13 - ((hole_choice + stone_count) `rem` 14)) final_board)) ++ [0] ++ (take (12 - ((hole_choice + stone_count) `rem` 14)) (drop (((hole_choice + stone_count) `rem` 14) + 1) final_board)) ++ [1 + (board !! (12 - (((hole_choice + stone_count) `rem` 14)))) + (board !! 13)]
                                    let which_user = 1
                                    let user1_part = take 6 final_board2
                                    let user2_part = take 6 (drop 7 final_board2)
                                    let user1_point = final_board2 !! 6
                                    let user2_point = final_board2 !! 13
                                
                                    putStrLn "        GAME MANGALA"

                                    if user2_point == 0
                                        then do
                                            putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                            if user1_point == 0
                                                then do
                                                    putStrLn ("  | " ++ board_printer user1_part)
                                                    game_engine final_board2 which_user
                                                else do
                                                    putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                    game_engine final_board2 which_user
                                        else do
                                            putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                            if user1_point == 0
                                                then do
                                                    putStrLn ("  | " ++ board_printer user1_part)
                                                    game_engine final_board2 which_user
                                                else do
                                                    putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                    game_engine final_board2 which_user
                            else if (((hole_choice - 1 + stone_count) `rem` 14) == 13) || ((stone_count == 0) && (hole_choice == 13))
                                then do
                                    let which_user = 2
                                    let user1_part = take 6 final_board
                                    let user2_part = take 6 (drop 7 final_board)
                                    let user1_point = final_board !! 6
                                    let user2_point = final_board !! 13
                                
                                    putStrLn "        GAME MANGALA"

                                    if user2_point == 0
                                        then do
                                            putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                            if user1_point == 0
                                                then do
                                                    putStrLn ("  | " ++ board_printer user1_part)
                                                    game_engine final_board which_user
                                                else do
                                                    putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                    game_engine final_board which_user
                                        else do
                                            putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                            if user1_point == 0
                                                then do
                                                    putStrLn ("  | " ++ board_printer user1_part)
                                                    game_engine final_board which_user
                                                else do
                                                    putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                    game_engine final_board which_user
                                else do
                                    let which_user = 1
                                    let user1_part = take 6 final_board
                                    let user2_part = take 6 (drop 7 final_board)
                                    let user1_point = final_board !! 6
                                    let user2_point = final_board !! 13
                                
                                    putStrLn "        GAME MANGALA"

                                    if user2_point == 0
                                        then do
                                            putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                            if user1_point == 0
                                                then do
                                                    putStrLn ("  | " ++ board_printer user1_part)
                                                    game_engine final_board which_user
                                                else do
                                                    putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                    game_engine final_board which_user
                                        else do
                                            putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                            if user1_point == 0
                                                then do
                                                    putStrLn ("  | " ++ board_printer user1_part)
                                                    game_engine final_board which_user
                                                else do
                                                    putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                    game_engine final_board which_user
                    else do
                        let stone_count = (board !! (hole_choice - 1)) - 1
                        let final_board = ((take (hole_choice - 1) board)) ++ [1] ++ (stone_alloc board turn_side 1  hole_choice ((board !! (hole_choice - 1) ))) ++ (drop (hole_choice + (board !! (hole_choice - 1) - 1)) board)
                        
                        if (((hole_choice - 1 + stone_count) `rem` 14) > 6) && (((hole_choice - 1 + stone_count) `rem` 14) < 12) && (((final_board !! ((hole_choice - 1 + stone_count) `rem` 14)) == 1) || ((stone_count == 0) && ((final_board !! ((hole_choice + stone_count) `rem` 14)) == 1)))
                            then do 
                                
                                let final_board1 = (take (11 - ((hole_choice - 1 + stone_count) `rem` 14 )) final_board) ++ [0] ++ (take (((hole_choice + stone_count) `rem` 14) - (13 - ((hole_choice + stone_count) `rem` 14))) (drop (13 - ((hole_choice + stone_count) `rem` 14)) final_board)) ++ [0] ++ (take (12 - ((hole_choice + stone_count) `rem` 14)) (drop (((hole_choice + stone_count) `rem` 14) + 1) final_board)) ++ [1 + (board !! (12 - (((hole_choice + stone_count) `rem` 14)))) + (board !! 13)]
                                let which_user = 1 
                                let user1_part = take 6 final_board1
                                let user2_part = take 6 (drop 7 final_board1)
                                let user1_point = final_board1 !! 6
                                let user2_point = final_board1 !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board1 which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board1 which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board1 which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board1 which_user
                        else if (((hole_choice - 1 + stone_count) `rem` 14) == 13) || ((stone_count == 0) && (hole_choice == 13))
                            then do
                                
                                let which_user = 2  
                                let user1_part = take 6 final_board
                                let user2_part = take 6 (drop 7 final_board)
                                let user1_point = final_board !! 6
                                let user2_point = final_board !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                            else do
                                
                                let which_user = 1
                                let user1_part = take 6 final_board
                                let user2_part = take 6 (drop 7 final_board)
                                let user1_point = final_board !! 6
                                let user2_point = final_board !! 13
                                
                                putStrLn "        GAME MANGALA"

                                if user2_point == 0
                                    then do
                                        putStrLn ("  | " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user
                                    else do
                                        putStrLn (show (user2_point :: Int) ++ "| " ++ (drop 3 (reverse (board_printer user2_part))) ++ " |")
                                        if user1_point == 0
                                            then do
                                                putStrLn ("  | " ++ board_printer user1_part)
                                                game_engine final_board which_user
                                            else do
                                                putStrLn ("  | " ++ board_printer user1_part ++ show (user1_point :: Int))
                                                game_engine final_board which_user

        

