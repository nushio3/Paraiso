{-# OPTIONS -Wall #-}
import Data.Graph.Inductive


g0, g :: Gr String Double
g0 = ([],1,"Start",[]) & empty

ps :: Node -> Adj Double
ps i = [(0, j) | j<-[i-k*k |k <- [1..i-1]] , 1<=j, j<=i-1]

contexts :: [Context String Double]
contexts = [(ps i,i,"n",[]) | i<-[2..30]]

g= foldl (flip (&)) g0 contexts 


main :: IO ()
main = do
  print g
  