{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
import Data.ListLike.IO 
import Data.ListLike.Text ()
import qualified Data.Text as T
import Prelude hiding (putStrLn)

greetings :: [T.Text]
greetings = ["おはよう", "こんばんわ", "さよなら"]

punkt :: T.Text
punkt = "、"

main :: IO ()
main = do
  putStrLn $ T.intercalate punkt greetings
  