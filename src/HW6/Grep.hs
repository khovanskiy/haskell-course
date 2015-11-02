module HW6.Grep(grep) where

import System.Environment
import Data.Array.IO
import System.Directory
import Data.List
import Data.Functor

type File = IOArray Int (String, Bool)

terminal :: File -> IO()
terminal file = do
    putStrLn "> Edit(E Number), Write to file(W OutputFileName), Quit(Q) ?"
    args <- getLine
    case words args of
        ("Q":_) -> do
            return ()
        ("E":strnum:_) -> do
            let number = read strnum
            line <- getLine
            putFileLn' file number line
            terminal file
        ("W":filename:_) -> do
            exists <- doesFileExist filename
            appendFile' file filename
            if exists
                then putStrLn $ "Changes appended to existing file `" ++ filename ++ "`"
                else putStrLn $ "File `" ++ filename ++ "` created, all changes saved"
            terminal file

readFile' :: FilePath -> (String -> Bool) -> IO File
readFile' filename predicate = do
    content <- readFile filename
    let strings = (\s -> (s, False)) <$> (filter predicate $ lines content)
    let count = length strings
    newListArray (1, count) strings

putFileLn' :: File -> Int -> String -> IO ()
putFileLn' file i s = do
    writeArray file i (s, True)

appendFile' :: File -> FilePath -> IO ()
appendFile' file filename = do
    elements <- getElems file
    appendFile filename $ unlines (fst <$> (filter (snd) elements))

logArray :: File -> Int -> Int -> IO ()
logArray array i m = do
    if i == m + 1
        then return ()
        else do
            (line, edited) <- readArray array i
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
--matchByWords :: String -> (String -> Bool)
--matchByWords word = any (==word) . words

grep :: IO()
grep = do
    args <- getArgs
    --putStrLn $ "Program arguments: " ++ show args
    let pattern = args !! 0
    let filename = args !! 1

    file <- readFile' filename (isInfixOf pattern)
    logFile file

    terminal file

