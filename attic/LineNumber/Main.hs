{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}

import GHC.Base

main :: IO ()
main = do
  putStrLn "hello"
  putStrLn $ "I am from line: " ++ show (__LINE__ :: Integer)
  assert (6*7 /= (42::Int)) $ putStrLn "the answer is lost forever."
  