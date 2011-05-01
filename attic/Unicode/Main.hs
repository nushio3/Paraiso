{-# OPTIONS -Wall #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T

greetings :: [T.Text]
greetings = map T.pack ["おはよう", "こんばんわ", "さよなら"]

punkt :: T.Text
punkt = T.pack "、"

main :: IO ()
main = do
  T.putStrLn $ T.intercalate punkt greetings
  