module Parse where

import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char                  (isSpace)


data Greymap = Greymap
    { greyWidth  :: Int
    , greyHeight :: Int
    , greyMax    :: Int
    , greyData   :: L.ByteString
    }

instance Show Greymap where
  show (Greymap w h m _) = "Greymap "
                              ++ "{width = "
                              ++ show w
                              ++ ", height = "
                              ++ show h
                              ++ ", gray = "
                              ++ show m
                              ++ "} "


parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 = undefined

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader = undefined

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat = undefined

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes = undefined

