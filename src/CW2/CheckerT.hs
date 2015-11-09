module CW2.CheckerT(check) where

--- N строк = пары чисел. Нужно проверить формат, использую трансформеры ---

import Data.Array.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Foldable(forM_)
import Text.Read
import Data.Monoid

data Result = Result [String] Bool
type File = IOArray Int String
type Context = WriterT Result IO ()

instance Monoid Result where
    mempty = Result [] True
    (Result a1 r1) `mappend` (Result a2 r2) = Result (a1 ++ a2) (r1 && r2)

check :: FilePath -> IO ()
check filename = do
    (_, Result l ok) <- runWriterT (checker filename)
    if ok
        then putStrLn $ "ok"
        else putStrLn $ unlines l

readFile' :: FilePath -> IO (File, Int)
readFile' filename = do
    content <- liftIO $ readFile filename
    let strings = lines content
    let count = length strings
    array <- newListArray (0, count - 1) strings
    return (array, count)

checkInts :: String -> Int -> Bool
checkInts s c = a == b && b == c where
    a = length $ words s
    b = length $ filter (\i -> (readMaybe i :: Maybe Int) /= Nothing) (words s)

checker :: FilePath -> Context
checker filename = do
    (file, count) <- liftIO $ readFile' filename
    forM_ [0..count - 1] $ \i -> do
        s <- liftIO $ readArray file i
        let ok = if i == 0 then checkInts s 1 else checkInts s 2
        if ok
            then writer((), Result ["прочитал строку " ++ show i ++ ", проверил, ok"]  True)
            else writer((), Result ["прочитал строку " ++ show i ++ ", проверил, ne ok"]  False)