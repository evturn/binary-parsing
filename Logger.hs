module Logger
    ( Logger
    , Log
    , runLogger
    , record
    ) where

type Log = [String]

newtype Logger a = Logger
    { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = undefined

record :: String -> Logger ()
record = undefined

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
