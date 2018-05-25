data Statelet = Empty|X|O deriving (Eq, Show) --the state of one square of the board
data Player =   Player1|Player2 deriving (Eq)

--[[0,1,2], [3,4,5], [6,7,8], [0,3,6], [1,4,7], [2,5,8], [0,4,8], [2,4,6]]

main = do
    putStrLn "Welcome to tic-tac-toe written in Haskell!"
    game (make 9 Empty) Player1
    
game state player = do
    putStrLn $ printState state ++ "\nPlayer " ++ (if player == Player1 then "1 (X)" else "2 (O)") ++ " please input your move."
    move <- getLine

    let new = doMove state player $ (read move :: Int) - 1 in
        if testForWin [[0,1,2], [3,4,5], [6,7,8], [0,3,6], [1,4,7], [2,5,8], [0,4,8], [2,4,6]] new (if player == Player1 then X else O)
        then
            putStrLn $ printState new ++  "\nPlayer " ++ (if player == Player1 then "1" else "2") ++ " has won!" 
        else if testAll [0..8] new (Empty /=) then
            putStrLn $ printState new ++ "\nIt's a draw!"
         else 
            game new $ if new == state then player else 
                case player of Player1 -> Player2
                               Player2 -> Player1

make :: (Integral a) => a -> b -> [b]
make 0 _ = []
make n y = y:make (n-1) y

testForWin :: [[Int]] -> [Statelet] -> Statelet -> Bool
testForWin (x:xs) y z = if testAll x y (z ==) then True else testForWin xs y z
testForWin [] _ _ = False

testAll :: (Eq a) => [Int] -> [a] -> (a -> Bool) -> Bool
testAll [] _ _ = True
testAll (x:xs) y z = if z $ y !! x then testAll xs y z else False

doMove :: [Statelet] -> Player -> Int -> [Statelet]
doMove state player place = 
    if state !! place == Empty then 
        set state place (if player == Player1 then X else O)
    else 
        state
    
printState :: [Statelet] -> String
printState [] = ""
printState (x:xs) = let l = length xs in
    (case l of 8 -> ""
               _ -> if l `mod` 3 == 2 then "\n---+---+---\n" else "|")
    ++ " " ++
    (case x of X     -> "X"
               O     -> "O"
               Empty -> show $ 9-l)
    ++ " " ++
    printState xs

set :: [a] -> Int -> a -> [a]
set [] _ _ = []
set (x:xs) y z = if y == 0
    then z:xs else x:set xs (y - 1) z