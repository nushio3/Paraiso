{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
import Data.ListLike
import Data.ListLike.IO
import qualified Data.ByteString.Lazy.Char8 as BS
import Prelude hiding (putStrLn)

greetings :: [BS.ByteString]
greetings = ["おはよう", "こんばんわ", "さよなら"]

punkt :: BS.ByteString
punkt = "、"

main :: IO ()
main = do
  putStrLn $ BS.intercalate punkt greetings
  