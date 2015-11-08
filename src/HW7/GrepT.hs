module HW7.GrepT(grep) where

import System.Environment
import Data.Array.IO
import System.Directory
import Data.List
import Data.Functor
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader

type File = IOArray Int (String, Bool)

type Context = ReaderT File (StateT (Int, Int) (WriterT [String] IO)) ()

terminal :: File -> IO ()
terminal file = do
    ((_, (ec, wc)), l) <- runWriterT (runStateT (runReaderT loop file) (0, 0))
    putStrLn $ "# Operations count"
    putStrLn $ "* Edit count  = " ++ show ec
    putStrLn $ "* Write count = " ++ show wc
    putStrLn $ "# Log"
    putStrLn $ unlines l

loop :: Context
loop = do
    putStrLn' $ "> Edit(E Number), Write to file(W OutputFileName), Quit(Q) ?"
    args <- getLine'
    case words args of
        ("Q":_) -> do
            return ()
        ("E":strnum:_) -> do
            let number = read strnum
            line <- getLine'
            putFileLn' number line
            loop
        ("W":filename:_) -> do
            exists <- doesFileExist' filename
            appendFile' filename
            if exists
                then putStrLn' $ "Changes appended to existing file `" ++ filename ++ "`"
                else putStrLn' $ "File `" ++ filename ++ "` created, all changes saved"
            loop

putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' = liftIO . putStrLn

getLine' :: (MonadIO m) => m String
getLine' = liftIO getLine

doesFileExist' :: (MonadIO m) => FilePath -> m Bool
doesFileExist' = liftIO . doesFileExist

readFile' :: FilePath -> (String -> Bool) -> IO File
readFile' filename predicate = do
    content <- readFile filename
    let strings = (\s -> (s, False)) <$> (filter predicate $ lines content)
    let count = length strings
    newListArray (1, count) strings

putFileLn' :: Int -> String -> Context
putFileLn' i s = do
    file <- ask
    liftIO $ writeArray file i (s, True)
    lift . lift $ writer ((), return $ "Line #" ++ show i ++ " has been modified")
    lift . modify $ \(ec, wc) -> (ec + 1, wc)

appendFile' :: FilePath -> Context
appendFile' filename = do
    file <- ask
    elements <- liftIO $ getElems file
    liftIO $ appendFile filename $ unlines (fst <$> (filter (snd) elements))
    lift . lift $ writer ((), return $ "Data has been written to " ++ filename)
    lift . modify $ \(ec, wc) -> (ec, wc + 1)

logArray :: File -> Int -> Int -> IO ()
logArray array i m = do
    if i == m + 1
        then return ()
        else do
            (line, _) <- readArray array i
            putStrLn $ show i ++ ". \"" ++ line ++ "\""
            logArray array (i + 1) m

logFile :: File -> IO()
logFile file = do
    (l, r) <- getBounds file
    if l <= r
        then logArray file l r
        else do
            putStrLn "No lines"

grep :: IO()
grep = do
    args <- getArgs
    --putStrLn $ "Program arguments: " ++ show args
    let pattern = args !! 0
    let filename = args !! 1

    file <- readFile' filename (isInfixOf pattern)
    logFile file

    terminal  file