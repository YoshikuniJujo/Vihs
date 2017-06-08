{-# LANGUAGE TupleSections #-}

module Ed (ed) where

import Control.Monad (unless)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import System.Console.Haskeline
import Text.Parsec
import Text.Parsec.String

data EdArgs = EdArgs {
	fileName :: String, buff :: [String], crrLine :: Int, saved :: Bool }
	deriving Show

ed :: [String] -> IO ()
ed args = do
	(bf, fp) <- case args of
		[] -> return ([], "")
		f : _ -> (, f) <$> createBuffer f
	cmd <- inputCmd
	ed' cmd $ EdArgs {fileName = fp, buff = bf, crrLine = 1, saved = True}

ed' :: Command -> EdArgs -> IO ()
ed' cmd edArgs = case cmdName cmd of
        'q' ->
                  unless (saved edArgs) $ do
                          putStrLn "?"
                          newCmd <- inputCmd
                          ed' newCmd edArgs {saved = cmdName newCmd == 'q'}
        'a' ->
                  insert >>= (\x -> inputCmd >>=
                  (`ed'` edArgs {buff = iCmd (buff edArgs) x $ fromMaybe (crrLine edArgs) (addr1 cmd) + 1, saved = False}))
        'i' ->
                  insert >>= (\x -> inputCmd >>=
                  (`ed'` edArgs {buff = iCmd (buff edArgs) x $ fromMaybe (crrLine edArgs) $ addr1 cmd, saved = False}))
        'd' ->
                  inputCmd >>=
                  (`ed'` edArgs {buff = deleteLine (buff edArgs) (fromMaybe (crrLine edArgs) $ addr1 cmd) (fromMaybe 1 $ addr2 cmd), saved = False})
        'l' -> do
                  printBuff cmd edArgs $ addDll $ buff edArgs
                  inputCmd >>= (`ed'` edArgs)
        'n' -> do
                  let infNo = map show (take (length $ buff edArgs) [1 :: Int, 2..])
                  printBuff cmd edArgs $ zipWith (++) (map (take 8 . (++ repeat ' ')) infNo) (addDll $ buff edArgs)
                  inputCmd >>= (`ed'` edArgs)
        'w' ->
                  if isNothing $ param cmd
                          then putStrLn "?"
                                  >> inputCmd >>= (`ed'` edArgs)
                          else buffToFile (fromJust (param cmd)) (buff edArgs)
                                  >> (print (length (unlines $ buff edArgs))
                                  >> inputCmd >>= (`ed'` edArgs {saved = True}))
        _ ->
                  putStrLn "?" >> inputCmd >>= (`ed'` edArgs)

inputCmd :: IO Command
inputCmd = setCmd . fromMaybe "" <$> runInputT defaultSettings (getInputLine "")

addDll :: [String] -> [String]
addDll = map (++"$")

printBuff :: Command -> EdArgs -> [String] -> IO ()
printBuff cmd edArgs allLines =
        putStr $ unlines $ drop
                (fromMaybe (crrLine edArgs) (addr1 cmd) - 1)
                (reverse (drop (length allLines - (fromMaybe 1 (addr1 cmd) + fromMaybe 1 (addr2 cmd) - 1)) $ reverse allLines))

iCmd :: [String] -> [String] -> Int -> [String]
iCmd bf bf2 line =
    take (line - 1) bf ++ bf2 ++ reverse (take (length bf - line + 1) (reverse bf))

insert :: IO [String]
insert = insert' [] False
    where
        insert' :: [String] -> Bool -> IO [String]
        insert' bf done
            | done = return bf
            | otherwise = do
                str <- getLine
                if str == "."
                    then insert' bf True
                    else insert' (bf ++ [str]) False

deleteLine :: [String] -> Int -> Int ->[String]
deleteLine str line times
  | line <= length str && line >= 0 && line - 1 <= length str =
    (take (line - 1) str) ++ (reverse . take ((length str) - line - times + 1) $ reverse str)
  | otherwise = str

createBuffer :: String -> IO [String]
createBuffer path = readFile path >>= \x -> return $ lines x

buffToFile :: String -> [String] -> IO ()
buffToFile path str = writeFile path $ unlines str

data Command = Command
  { addr1   :: Maybe Int
  , addr2   :: Maybe Int
  , cmdName :: Char
  , param   :: Maybe String
  } deriving Show 

setCmd :: String -> Command
setCmd str = Command
  (if addrs == []
    then Nothing
    else Just (addrs !! 0))
  (if length addrs < 2
    then Nothing
    else Just (addrs !! 1))
  (last $ (words str) !! 0)
  (if length (words str) < 2
    then Nothing
    else (Just $ (words str) !! 1))
      where
        addrs = (parseIntList $ init $ (words str) !! 0)

parseInt :: Parser Int
parseInt = do
  value <- many1 digit
  return (read value)

parseText :: Parser [Int]
parseText = parseInt `sepBy1` (char ',')

parseIntList :: String -> [Int]
parseIntList input
  = case (parse parseText "" input) of
    Left _err -> []
    Right x -> x
