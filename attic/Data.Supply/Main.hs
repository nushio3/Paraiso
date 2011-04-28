{-# OPTIONS -Wall #-}

import Data.Supply


main :: IO ()
main = do
  supps <- fmap split $ newSupply 0 (1+)
  print $ map supplyValue $ take 10 $ supps
