module EXAM.Ls(ls) where

import Data.Array.IO
import Data.IORef
import System.Directory
import System.Environment
import Control.Monad (when)
import Control.Monad.Loops (whileM_)
import Data.List (find)

type Entry = String
type Entries = IOArray Int Entry

data Args = Args {a :: Bool, l :: Bool, curPath :: FilePath} deriving (Show, Eq)

defaultArgs :: Args
defaultArgs = Args False True "."

sortEntries :: Entries -> IO Entries
sortEntries entries = do
    (_, n) <- getBounds entries
    i <- newIORef 0
    j <- newIORef 0
    m <- newIORef 0
    whileM_ (do jj <- readIORef j; return $ jj < n) $ do
        jj <- readIORef j
        ---print $ "j = " ++ show jj
        writeIORef m jj
        writeIORef i (jj + 1)
        whileM_ (do ii <- readIORef i; return $ ii <= n) $ do
            ii <- readIORef i
            ai <- readArray entries ii
            mm <- readIORef m
            am <- readArray entries mm
            when (ai < am) $ do
                writeIORef m ii
            writeIORef i (ii + 1)
        mm <- readIORef m
        when (mm /= jj) $ do
            ---print $ "swap " ++ show mm ++ " " ++ show jj
            aj <- readArray entries jj
            am <- readArray entries mm
            writeArray entries mm aj
            writeArray entries jj am
        writeIORef j (jj + 1)
    ---print $ "N " ++ show n
    return entries

getEntries :: FilePath -> IO Entries
getEntries filepath = do
    entries <- getDirectoryContents filepath
    let count = length entries
    newListArray (0, count - 1) entries

showEntries :: Entries -> Args -> IO ()
showEntries entries args = do
    (_, n) <- getBounds entries
    i <- newIORef (0 :: Int)
    putStrLn $ "итого " ++ show (n + 1)
    whileM_ (do ii <- readIORef i; return $ ii <= n) $ do
        ii <- readIORef i
        ai <- readArray entries ii
        when (a args || ai /= "." && ai /= "..") $ do
            if l args
                then do
                    putStrLn $ ai
                else do
                    putStr $ ai ++ (if ii < n then ", " else "")
        writeIORef i (ii + 1)
    putStrLn ""

parseArguments :: IO Args
parseArguments = do
    args <- getArgs
    ---print $ "Arguments " ++ show args
    let arg_a = any (=="-a") args
    ---let arg_m = any (=="-m") args
    let arg_l = any (=="-l") args
    ---when (arg_m && arg_l) $ error "Нельзя вывести список и таблицу"
    execPath <- getCurrentDirectory
    let path = case find (\s -> head s /= '-') args of
                    Just x  -> x
                    Nothing -> execPath
    ---execPath <- getCurrentDirectory
    --let curPath = if p then execPath else head args
    return $ Args arg_a arg_l path

ls :: IO ()
ls = do
    args@(Args _ _ path) <- parseArguments
    ---print $ args
    ---print $ "Current path " ++ path
    ---list <- getDirectoryContents path
    unsorted <- getEntries path
    sorted <- sortEntries unsorted
    ---print $ entries
    ---print $ "Entries " ++ show list
    ---putStrLn "Files"
    showEntries sorted args
    ---for_ list $ \i -> do
    ---    print $ i