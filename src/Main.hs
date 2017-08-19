module Main where

import Data.List
import Data.Char
import Data.Maybe
import Data.Ratio

import Text.Read (readMaybe)

import Control.Concurrent
import Control.Monad

import System.IO
import System.Directory
import System.Exit
import System.Environment
import System.Process

data Direction = DUp | DLeft | DRight | DDown deriving (Show, Eq)
type Variable = (String, Value)
data MState = MState {
    vars :: [Variable],
    stack :: [Value],
    pos :: Maybe (Int, Int),
    direction :: Direction,
    currentNumber :: String,
    currentString :: String,
    currentVar :: String,
    dataTransferPoints :: [((Int, Int), MVar Value)],
    inString :: Bool,
    inVar :: Bool,
    threadVal :: Value, -- A value specific to each thread
    callStack :: [Maybe (Int, Int)]
}

data Value = StrVal String
        | DoubleVal Double
        | IntVal Integer
        | ListVal [Value]
        | RefVal String -- Reference to variable
        | FuncRef String
        | BoolVal Bool
        | RatioVal (Ratio Integer)
        deriving (Eq)

instance Show Value where
    show (StrVal s) = s
    show (DoubleVal n) = show n
    show (IntVal i) = show i
    show (ListVal l) = show l
    show (RefVal varname) = "$" ++ varname ++ "$"
    show (BoolVal b) = show b
    show (FuncRef funname) = funname

instance Ord Value where
    compare (IntVal v1) (IntVal v2) = compare v1 v2
    compare (IntVal v1) (DoubleVal v2) = compare (fromIntegral v1) v2
    compare (DoubleVal v1) (IntVal v2) = compare v1 (fromIntegral v2)
    compare (DoubleVal v1) (DoubleVal v2) = compare v1 v2
    compare (ListVal v1) (ListVal v2) = compare v1 v2
    compare (StrVal v1) (StrVal v2) = compare v1 v2
    compare (BoolVal b1) (BoolVal b2) = compare b1 b2
    compare v1 v2 = incompatible "comparison" [v1, v2]

typeof :: Value -> String
typeof (StrVal _) = "string"
typeof (DoubleVal _) = "double"
typeof (IntVal _) = "integer"
typeof (ListVal _) = "list"
typeof (RefVal _) = "variable reference"

push :: MState -> Value -> MState
push st val = st {stack=val:(stack st)}

pop :: MState -> (Value, MState)
pop st = (head $ stack st, st {stack=tail $ stack st})

incompatible :: String -> [Value] -> a -- Raise incompatible types error
incompatible funcname values = error $ "Incompatible type(s) for " ++ funcname ++ ": " ++ intercalate " and " types ++ "."
    where types = map typeof values

toChar :: Value -> Char
toChar (IntVal i) = chr $ fromIntegral i
toChar (StrVal s) = head s
toChar v = incompatible "string item assignment" [v]

(!+!) :: Value -> Value -> Value -- Addition
(!+!) (StrVal s1) (StrVal s2) = StrVal $ s1 ++ s2
(!+!) (ListVal l1) (ListVal l2) = ListVal $ l1 ++ l2
(!+!) (IntVal i1) (IntVal i2) = IntVal $ i1 + i2
(!+!) (DoubleVal d1) (DoubleVal d2) = DoubleVal $ d1 + d2
(!+!) (IntVal i1) (DoubleVal d2) = DoubleVal $ (fromIntegral i1) + d2
(!+!) (DoubleVal d1) (IntVal i2) = DoubleVal $ d1 + (fromIntegral i2)
(!+!) (BoolVal b1) (BoolVal b2) = BoolVal $ b1 || b2 -- addition for bools = or
(!+!) v1 v2 = incompatible "addition" [v1, v2]

(!-!) :: Value -> Value -> Value
(!-!) (IntVal i1) (IntVal i2) = IntVal $ i1 - i2
(!-!) (DoubleVal d1) (DoubleVal d2) = DoubleVal $ d1 - d2
(!-!) (IntVal i1) (DoubleVal d2) = DoubleVal $ (fromIntegral i1) - d2
(!-!) (DoubleVal d1) (IntVal i2) = DoubleVal $ d1 - (fromIntegral i2)
(!-!) v1 v2 = incompatible "subtraction" [v1, v2]

(!*!) :: Value -> Value -> Value
(!*!) (IntVal i1) (IntVal i2) = IntVal $ i1 * i2
(!*!) (DoubleVal d1) (DoubleVal d2) = DoubleVal $ d1 * d2
(!*!) (IntVal i1) (DoubleVal d2) = DoubleVal $ (fromIntegral i1) * d2
(!*!) (DoubleVal d1) (IntVal i2) = DoubleVal $ d1 * (fromIntegral i2)
(!*!) (StrVal s1) (IntVal i2)  = StrVal  $ concat $ replicate (fromIntegral i2) s1
(!*!) (ListVal l1) (IntVal i2) = ListVal $ concat $ replicate (fromIntegral i2) l1
(!*!) (BoolVal b1) (BoolVal b2) = BoolVal $ b1 && b2 -- multiplication for bools = and
(!*!) v1 v2 = incompatible "multiplication" [v1, v2]

(!/!) :: Value -> Value -> Value
(!/!) (IntVal i1) (IntVal i2) = IntVal $ i1 `div` i2
(!/!) (DoubleVal d1) (DoubleVal d2) = DoubleVal $ d1 / d2
(!/!) (IntVal i1) (DoubleVal d2) = DoubleVal $ (fromIntegral i1) / d2
(!/!) (DoubleVal d1) (IntVal i2) = DoubleVal $ d1 / (fromIntegral i2)
(!/!) v1 v2 = incompatible "division" [v1, v2]

(!=!) :: Value -> Value -> Value
(!=!) (IntVal i1) (IntVal i2) = BoolVal $ i1 == i2
(!=!) (DoubleVal d1) (DoubleVal d2) = BoolVal $ d1 == d2
(!=!) (BoolVal b1) (BoolVal b2) = BoolVal $ b1 == b2
(!=!) (StrVal s1) (StrVal s2) = BoolVal $ s1 == s2
(!=!) (ListVal l1) (ListVal l2) = BoolVal $ ((length l1) == (length l2)) && (all id $ map (\(BoolVal b)->b) $ zipWith (!=!) l1 l2)
(!=!) v1 v2 = incompatible "equality" [v1, v2]

(!:!) (ListVal l) val = ListVal $ val:l
(!:!) (StrVal s)  val = StrVal  $ (toChar val):s
(!:!) v1 v2 = incompatible "prepend" [v1, v2]

(!^!) :: Value -> Value -> Value -- Exponentiation, XOR, and list/string at position
(!^!) (IntVal v1) (IntVal v2) = IntVal $ v1 ^ v2
(!^!) (DoubleVal v1) (IntVal v2) = DoubleVal $ v1 ^ v2
(!^!) (IntVal v1) (DoubleVal v2) = DoubleVal $ (fromIntegral v1) ** v2
(!^!) (DoubleVal v1) (DoubleVal v2) = DoubleVal $ v1 ** v2
(!^!) (ListVal v1) (IntVal v2) = v1 !! (fromIntegral v2)
(!^!) (StrVal v1) (IntVal v2) = IntVal $ fromIntegral $ ord $ v1 !! (fromIntegral v2)
(!^!) (BoolVal v1) (BoolVal v2) = BoolVal $ (v1 || v2) && (not $ v1 && v2) -- XOR
(!^!) v1 v2 = incompatible "^" [v1, v2]

(!>!) :: Value -> Value -> Value
(!>!) a = BoolVal . (a>)
(!<!) :: Value -> Value -> Value
(!<!) a = BoolVal . (a<)

untilM :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
untilM check func val = do
    is <- check val
    if is then return val else do
        res <- func val
        untilM check func res

append :: Value -> Value -> Value -- Append to list/str
append (ListVal l) val = ListVal $ l ++ [val]
append (StrVal s)  val = StrVal  $ s ++ [toChar val]

set :: [Variable] -> String -> Value -> [Variable]
set vars varname varval = (varname, varval):(filter ((/=varname) . fst) vars)

setAt :: MState -> Int -> String -> Value -> MState
setAt state pos li val = state {vars=set (vars state) li newli}
    where newli = case lookup li (vars state) of
                        Nothing -> error $ "Variable not found: " ++ li
                        Just (StrVal s) ->  StrVal $  take pos s ++ [toChar val] ++ drop (pos+1) s
                        Just (ListVal l) -> ListVal $ take pos l ++        [val] ++ drop (pos+1) l

getListRef :: [Variable] -> String -> Value
getListRef vs s = case lookup liname vs of
                        Just (ListVal v) -> v !! pos
                        Nothing -> error $ "List not found: " ++ liname
    where (liname, _:pos') = splitAt (fromJust $ elemIndex '^' s) s
          pos = read pos'

equals :: MState -> Value -> Value -> MState
equals state (RefVal r) x
    | '^' `elem` r = let (li, _:pos) = splitAt (fromJust $ elemIndex '^' r) r in setAt state (read pos) li x
    | otherwise = state {vars=set (vars state) r x}
equals state a b = push state $ a !=! b

at :: MState -> MState
at state@(MState {stack=(RefVal varname):rest}) = push (state {stack=rest}) (case lookup varname (vars state) of
    Nothing -> (if '^' `elem` varname then
        getListRef (vars state) varname
        else if '(' `elem` varname && ')' `elem` varname then
        FuncRef varname
        else error $ "Variable not found: " ++ varname)
    Just val -> val)

mtostr :: Value -> Value
mtostr (IntVal i) = StrVal $ show i
mtostr (DoubleVal d) = StrVal $ show d
mtostr (StrVal s) = StrVal s
mtostr (ListVal l) = StrVal $ "[" ++ intercalate "," (map ((\(StrVal s)->s) . mtostr) l) ++ "]"
mtostr v1 = incompatible "to_string" [v1]

mtodouble :: Value -> Value
mtodouble (StrVal s) = DoubleVal $ read s
mtodouble (DoubleVal d) = DoubleVal d
mtodouble (IntVal i) = DoubleVal $ fromIntegral i
mtodouble v1 = incompatible "to_double" [v1]

mtoint :: Value -> Value
mtoint (StrVal s) = DoubleVal $ read s
mtoint (DoubleVal d) = IntVal $ floor d
mtoint (IntVal i) = IntVal i
mtoint v1 = incompatible "to_int" [v1]

mnot :: Value -> Value
mnot (DoubleVal d) = DoubleVal $ negate d -- NOT bitwise not
mnot (IntVal i) = IntVal $ negate i
mnot (BoolVal b) = BoolVal $ not b
mnot v = incompatible "not" [v]

mlength :: Value -> Value
mlength (StrVal s) =  IntVal $ fromIntegral $ length s
mlength (ListVal l) = IntVal $ fromIntegral $ length l
mlength v = incompatible "length" [v]


mord :: Value -> Value
mord (StrVal s) =  IntVal $ fromIntegral $ ord $ head s
mord v = incompatible "ord" [v]

mchr :: Value -> Value
mchr (IntVal i) =  StrVal $ (\x->[x]) $ chr $ fromIntegral i
mchr v = incompatible "chr" [v]

mreadFile :: Value -> IO Value
mreadFile (StrVal path) = fmap StrVal $ readFile path
mreadFile v = incompatible "read file" [v]

mwriteFile :: Value -> Value -> IO ()
mwriteFile (StrVal path) (StrVal contents) = writeFile path contents
mwriteFile v1 v2 = incompatible "write file" [v1, v2]


mappendFile :: Value -> Value -> IO ()
mappendFile (StrVal path) (StrVal contents) = appendFile path contents
mappendFile v1 v2 = incompatible "append file" [v1, v2]

mcopyFile :: Value -> Value -> IO ()
mcopyFile (StrVal path) (StrVal newpath) = copyFile path newpath
mcopyFile v1 v2 = incompatible "copy file" [v1, v2]

mmoveFile :: Value -> Value -> IO ()
mmoveFile (StrVal path) (StrVal newpath) = renameFile path newpath
mmoveFile v1 v2 = incompatible "move file" [v1, v2]

mdeleteFile :: Value  -> IO ()
mdeleteFile (StrVal path) = removeFile path
mdeleteFile v = incompatible "delete file" [v]

mexec :: Value -> IO Value
mexec (StrVal command) = fmap StrVal $ (fmap (\(_, Just a, _, _)->a) $ createProcess $ (shell command){ std_out = CreatePipe }) >>= hGetContents
    where (cmd:args) = words command
mexec v = incompatible "execute" [v]

verify :: [[Char]] -> (Int, Int) -> Maybe (Int, Int)
verify code (x, y)
    | x < 0 || y < 0 = Nothing
    | y >= length code = Nothing
    | x >= length (code !! y) = Nothing
    | otherwise = Just (x, y)

move :: [[Char]] ->  Direction -> (Int, Int) -> Maybe (Int, Int)
move code DUp (x, y) = verify code (x, y-1)
move code DLeft (x, y) = verify code (x-1, y)
move code DDown (x, y) = verify code (x, y+1)
move code DRight (x, y) = verify code (x+1, y)

infixl 9 >$
(>$) :: a -> (a -> b) -> b
(>$) x f = f x

escape :: Char -> String
escape char = case char of
    't' -> "\t"
    'v' -> "\v"
    'r' -> "\r"
    'n' -> "\n"
    '"' -> "\""
    '\\' -> "\\"

findSublist :: Eq a => [a] -> [a] -> Maybe Int
findSublist subli li = findIndex (isPrefixOf subli) $ tails li


findFunc :: [String] -> String -> Maybe (Int, Int)
findFunc code varname = liftM2 (,) (y >>= getx) y
    where y = findIndex (isInfixOf varname) code
          getx y' = fmap ((length varname)+) $ findSublist varname (code !! y')

findRs :: [[Char]] -> [(Int, Int)]
findRs src = [(x,y) | y <- [0..length src - 1], x <- [0..length (src !! y)-1], (src !! y) !! x == 'R']

getDTPs :: [[Char]] -> IO [((Int, Int), MVar Value)]  -- Find R points
getDTPs src = do
    let rs = findRs src
    mvars <- sequence $ replicate (length rs) newEmptyMVar
    return $ zip rs mvars

initialState :: [[Char]] -> IO MState
initialState src = do
    dtps <- getDTPs src
    return $ MState {vars=[],
        stack=[], pos=Just (0,0), direction=DRight, currentNumber="", currentString="", currentVar="", inString=False,
        inVar=False, callStack=[], dataTransferPoints=dtps, threadVal=IntVal 0}

getJust :: String -> Maybe a -> a
getJust s Nothing  = error s
getJust _ (Just x) = x


step :: [[Char]] -> MState -> IO MState
step code state = do
    let Just (x, y) = pos state
    let dir = direction state

    let char = (code !! y) !! x
    let newpos = move code dir (x, y)

    let cn = currentNumber state
    let cs = currentString state
    let cv = currentVar state
    let addNumber num st = st {currentNumber = currentNumber st ++ [num]}
    let defaultmove st = return $ st {pos=newpos}
    let newnew = newpos >>= move code dir
    let movetwice st = return $ st {pos=newnew}
    let quit = return $ state {pos=Nothing}
    let pushNumber constructor err = case readMaybe $ currentNumber state of
            Just x -> state {currentNumber="", stack=(constructor x):(stack state)} >$ defaultmove
            Nothing -> (hPutStrLn stderr $ err ++ currentNumber state) >> quit

    let backslash = case newpos of
            Just (x, y) -> state {currentString=cs ++ (escape $ (code !! y) !! x)} >$ movetwice
            Nothing -> defaultmove state


    let binary op = let (v2,state') = pop state; (v1,state'') = pop state' in push state'' (v1 `op` v2) >$ defaultmove
    let binaryM op = let (v2,state') = pop state; (v1,state'') = pop state' in (v1 `op` v2) >> (state'' >$ defaultmove)
    let unaryM  op = let (v1, state') = pop state in (fmap (push state') (op v1)) >>= defaultmove
    let unaryM_ op = let (v1, state') = pop state in (op v1) >> (state' >$ defaultmove)
    let unary op = let (v1, state') = pop state in push state' (op v1) >$ defaultmove
    let callst = callStack state
    let forkHere t = forkIO (run (state {pos=move code DRight (x,y), threadVal=t}) code)
    let fork dir   = forkIO (run (state {pos=move code dir (x,y),  direction=dir}) code)
    let send dir = let (v, state') = pop state in (putMVar (snd $ head $ filter ((==(getJust "Error: problematic u/v" $ move code dir (x, y))) . fst) $ dataTransferPoints state) v) >> state' >$ defaultmove

    let process = case char of
            -- Motion
            '>' -> return $ state {direction=DRight, pos=move code DRight (x, y)}
            '<' -> return $ state {direction=DLeft, pos=move code DLeft (x, y)}
            'V' -> return $ state {direction=DDown, pos=move code DDown (x, y)}
            '^' -> return $ state {direction=DUp, pos=move code DUp (x, y)}
            '#' -> state >$ movetwice
            'K' -> quit -- Kill pointer

            -- Stack-related operations
            '~' -> let (v1, state') = pop state; (v2, state'') = pop state' in state >$ flip push v1 >$ flip push v2 >$ defaultmove -- Flip top 2 stack items
            'D' -> let (v, state') = pop state in state >$ flip push v >$ defaultmove -- Duplicate top stack item
            'P' -> let (_, state') = pop state in state' >$ defaultmove -- Pop


            -- Pushing numbers
            '0' -> state >$ addNumber '0' >$ defaultmove
            '1' -> state >$ addNumber '1' >$ defaultmove
            '2' -> state >$ addNumber '2' >$ defaultmove
            '3' -> state >$ addNumber '3' >$ defaultmove
            '4' -> state >$ addNumber '4' >$ defaultmove
            '5' -> state >$ addNumber '5' >$ defaultmove
            '6' -> state >$ addNumber '6' >$ defaultmove
            '7' -> state >$ addNumber '7' >$ defaultmove
            '8' -> state >$ addNumber '8' >$ defaultmove
            '9' -> state >$ addNumber '9' >$ defaultmove
            '.' -> state >$ addNumber '.' >$ defaultmove
            'n' -> state >$ addNumber '-' >$ defaultmove -- negative
            'F' -> pushNumber DoubleVal "Error: Not a number: "-- push double (DoubleVal)
            'I' -> pushNumber IntVal "Error: Not an integer: "-- push integer (IntVal)


            -- Operations
            '+' -> binary (!+!)
            '-' -> binary (!-!)
            '*' -> binary (!*!)
            '/' -> binary (!/!)
            '`' -> binary (!^!)
            'G' -> binary (!>!)
            'L' -> binary (!<!)
            '=' -> let (v1, state') = pop state; (v2, state'') = pop state' in equals state'' v2 v1 >$ defaultmove

            -- Type conversion
            's' -> unary mtostr
            'd' -> unary mtodouble
            'i' -> unary mtoint

            -- Strings
            '"' -> defaultmove $ if inString state then state {currentString="", inString=False, stack=(StrVal cs):(stack state)} else state {inString=True}
            '\\' -> if inString state then backslash else binary (\x y->BoolVal $ not $ (\(BoolVal b)->b) $ x !=! y)
            'l' -> unary mlength
            'o' -> unary mord -- Ord
            'h' -> unary mchr -- Chr

            -- vars
            '$' -> defaultmove $ if inVar state then state {currentVar="", inVar=False, stack=(RefVal cv):(stack state)} else state {inVar=True}
            '@' -> state >$ at >$ defaultmove -- Get value at reference

            -- Bools
            't' -> push state (BoolVal True) >$ defaultmove -- push true
            'f' -> push state (BoolVal False) >$ defaultmove -- push false
            '!' -> unary mnot -- Not
            'C' -> let (BoolVal b, state') = pop state in return $ state' {pos=move code (if b then DUp else DDown) (x,y)}-- condition (move up if true, down if false)

            -- Lists
            'e' -> push state (ListVal []) >$ defaultmove -- Empty list
            'E' -> let (ListVal li, state') = pop state in do
                sequence $ map forkHere li
                threadDelay 10000 -- Make sure threads are created before this one is killed
                return $ state' {pos=move code DDown (x,y)} -- For each in list
            ':' -> binary (!:!) -- Prepend (like Haskell's :)
            'a' -> binary append -- Append to list/str

            -- Functions
            'c' -> let (FuncRef f, state') = pop state in return $ state' {pos=findFunc code ("[" ++ (tail $ init f) ++ "]"), callStack=(move code DRight (x,y)):callst} -- Call
            'r' -> let (goto:rest) = callst in return $ state {pos=goto, callStack=rest} -- Return

            -- Concurrency
            '|' -> fork DUp >> (return $ state {pos=move code DDown (x,y), direction=DDown})
            'R' -> do
                val <- takeMVar $ snd $ head $ filter ((==(x, y)) . fst) $ dataTransferPoints state
                push state val >$ defaultmove
            'v' -> send DDown
            'u' -> send DUp
            'g' -> state >$ flip push (threadVal state) >$ defaultmove

            -- IO
            'p' -> (putStr $ show $ head $ stack state) >> (hFlush stdout) >> (state {stack=tail $ stack state} >$ defaultmove) -- print
            'S' -> (putStrLn "---") >> (mapM_ print $ stack state) >> (putStrLn "----") >> (state >$ defaultmove)-- Show stack
            'N' -> (putStr "\n") >> (defaultmove state)
            'U' -> liftM2 push (return state) (fmap (StrVal . (\x->[x])) getChar) >>= defaultmove-- User input
            'M' -> binaryM mmoveFile
            'Y' -> binaryM mcopyFile
            'X' -> unaryM_ mdeleteFile

            '?' -> unaryM mreadFile
            'W' -> binaryM mwriteFile
            'A' -> binaryM mappendFile
            'x' -> unaryM mexec

            _ -> state >$ defaultmove

    if inString state && (not$char `elem` "\"\\") then state {currentString=cs ++ [char]} >$ defaultmove else
        if inVar state && (char /= '$') then state {currentVar=cv ++ [char]} >$ defaultmove else process


run :: MState -> [[Char]] -> IO ()
run state code = do
    untilM (return . (==Nothing) . pos) (step code) state
    return ()

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    args <- getArgs
    progname <- getProgName
    case args of
        [source] -> let src = fmap (filter (not . null) . lines) (readFile source) in do
            src' <- src
            let imports = map (unwords . tail . words) $ takeWhile ((=="using") . head . words) src'
            importsContents <- mapM (fmap (filter (not . null) . lines) . readFile) imports
            let src'' = (dropWhile ((=="using") . head . words) src') ++ concat importsContents
            is <- initialState src''
            run is src''
        _ -> do
            hPutStrLn stderr $ "Error: Usage: " ++ progname ++ " [source file]"
            exitFailure
