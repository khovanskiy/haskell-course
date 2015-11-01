module HW6.Grep(grep) where

import System.Environment
import Data.Functor
import Data.Array.IO
import Data.List

type File = IOArray Int String

terminal :: File -> IO()
terminal file = do
    putStrLn "> Edit(E Number), Write to file(W OutputFileName), Quit(Q) ?"
    args <- getLine
    case words args of
        ("Q":_) -> do
            return ()
        ("E":strnum:_) -> do
            let number = read strnum :: Int
            line <- getLine
            putFileLn' file number line
            terminal file
        ("W":filename:_) -> do
            putStrLn $ "File `" ++ filename ++ "` created, all changes saved | Changes appended to existing file"
            appendFile' file filename
            --putStrLn "Current lines:"
            --logFile file
            terminal file

readFile' :: FilePath -> (String -> Bool) -> IO File
readFile' filename predicate = do
    content <- readFile filename
    let strings = filter predicate $ lines content
    let count = length strings
    newListArray (1, count) strings

putFileLn' :: File -> Int -> String -> IO ()
putFileLn' file i s = do
    writeArray file i s

appendFile' :: File -> FilePath -> IO ()
appendFile' file filename = do
    elements <- getElems file
    let actions = appendFile filename <$> (++"\n") <$> elements
    sequence_ actions

logArray :: IOArray Int String -> Int -> Int -> IO ()
logArray array i m = do
    if i == m + 1
        then return ()
        else do
            line <- readArray array i
            putStrLn $ show i ++ ". \"" ++ line ++ "\""
            logArray array (i + 1) m

logFile :: File -> IO()
logFile file = do
    (l, r) <- getBounds file
    if l <= r
        then logArray file l r
        else do
            putStrLn "No lines"

---newFile :: FilePath -> File
---newFile filename = do
---    content <- readFile filename
---    let strings = newIORef <$> lines content
---    return strings
---
---readFileLn :: File -> Int -> IO String
---readFileLn file index = do
---    content <- file
---    ref <- content !! index
---    value <- readIORef ref
---    return value
---
---writeFileLn :: File -> Int -> String -> IO()
---writeFileLn file index str = do
---    content <- file
---    ref <- content !! index
---    writeIORef ref str

grep :: IO()
grep = do
    args <- getArgs
    let pattern = args !! 0
    let filename = args !! 1

    --content <- readFile filename
    --let strings = lines content
    file <- readFile' filename (isInfixOf pattern)

    logFile file
    --arr <- newArray (1, 10) "a" :: IO (IOArray Int String)
    --a <- readArray arr 1
    --print a
    --str <- readFileLn file 0
    --print str
    --print array
    --ref <- array !! 0
    --a <- readIORef ref
    --writeIORef ref "abc"
    --b <- readIORef ref
    --print $ a ++ " " ++ b
    --putStrLn $ "Program argument: " ++ show args
    --print strings
    terminal file

