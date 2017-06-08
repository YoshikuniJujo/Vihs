{-# LANGUAGE TupleSections #-}

module Ed (ed) where

import Data.Maybe (fromJust, fromMaybe, isNothing)
import System.Console.Haskeline
import Text.Parsec
import Text.Parsec.String

data EdArgs = EdArgs {
	fileName :: String,
	buff :: [String],
	crrLine :: Int,
	saved :: Bool,
	tryQuitOnce :: Bool }
	deriving Show

initialEdArgs :: EdArgs
initialEdArgs = EdArgs {
	fileName = "",
	buff = [],
	crrLine = 1,
	saved = True,
	tryQuitOnce = False }

ed :: [String] -> IO ()
ed args = do
	(bf, fp) <- case args of
		[] -> return ([], "")
		f : _ -> (, f) <$> createBuffer f
	doWhile initialEdArgs { fileName = fp, buff = bf } ed'

doWhile :: Monad m => a -> (a -> m (Maybe a)) -> m ()
doWhile x act = act x >>= maybe (return ()) (`doWhile` act)

ed' :: EdArgs -> IO (Maybe EdArgs)
ed' edArgs_ = do
	cmd <- inputCmd
	case cmdName cmd of
		'q'	| saved edArgs_ || tryQuitOnce edArgs_ -> return Nothing
			| otherwise -> do
				putStrLn "?"
				let newArgs = edArgs_ { tryQuitOnce = True }
				return $ Just newArgs
		'a' -> do
			x <- insert
			let newArgs = edArgs {
				buff = iCmd (buff edArgs) x
					$ fromMaybe (crrLine edArgs) (addr1 cmd) + 1,
				saved = False }
			return $ Just newArgs
		'i' -> do
			x <- insert
			let newArgs = edArgs {
				buff = iCmd (buff edArgs) x
					$ fromMaybe (crrLine edArgs) $ addr1 cmd,
				saved = False }
			return $ Just newArgs
		'd' -> do
			let newArgs = edArgs {
				buff = deleteLine
					(buff edArgs)
					(fromMaybe (crrLine edArgs) $ addr1 cmd)
					(fromMaybe 1 $ addr2 cmd),
				saved = False }
			return $ Just newArgs
		'l' -> do
			printBuff cmd edArgs $ addDll $ buff edArgs
			return $ Just edArgs
		'n' -> do
			let infNo = map show (take (length $ buff edArgs) [1 :: Int, 2..])
			printBuff cmd edArgs $ zipWith (++) (map (take 8 . (++ repeat ' ')) infNo) (addDll $ buff edArgs)
			return $ Just edArgs
		'w' -> do
			newArgs <- if isNothing $ param cmd
			then do	putStrLn "?"
				return edArgs
			else do	buffToFile (fromJust (param cmd)) (buff edArgs)
				print . length . unlines $ buff edArgs
				return edArgs {saved = True}
			return $ Just newArgs
		_ -> do	putStrLn "?"
			let newArgs = edArgs
			return $ Just newArgs
	where
	edArgs = edArgs_ { tryQuitOnce = False }

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
