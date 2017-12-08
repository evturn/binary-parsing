module Logger
    ( Logger
    , Log
    , runLogger
    , record
    ) where

import           Control.Monad

type Log = [String]

newtype Logger a = Logger
    { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
  return a = Logger (a, [])
  m >>= k = let (a, w) = execLogger m
                n      = k a
                (b, x) = execLogger n
             in Logger (b, w ++ x)

globToRegex :: String -> Logger String
globToRegex cs = globToRegex' cs >>= \ds -> return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?':cs) =
  record "any" >>
  globToRegex' cs >>= \ds ->
  return ('.':ds)
globToRegex' ('*':cs) =
  record "kleene star" >>
  globToRegex' cs >>= \ds ->
  return (".*" ++ ds)
globToRegex' ('[':'!':c:cs) =
  record "character class, negative" >>
  charClass cs >>= \ds ->
  return ("^" ++ c : ds)
globToRegex' ('[':c:cs) =
  record "character class" >>
  charClass cs >>= \ds ->
  return ("[" ++ c : ds)
globToRegex' ('[':_) = fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs)   = (c:) `liftM` charClass cs

escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\', c]
    | otherwise           = return [c]
  where
    regexChars = "\\+()^$.{}]|"
