import Paraiso
import Data.Complex


main = do
  args <- getArgs
  let arch = if "--cuda" `elem` args then
               CUDA 128 128
             else
               X86
  putStrLn $ compile arch code
    where
      code = do
         parallel 16384 $ do
           r <- allocate
           x <- allocate
           r =$ Rand (0.0::Double) 4.0
           x =$ Rand 0 1
           cuda $ do
             sequential 65536 $ do
               r =$ r * x * (1-x)
           output [r,x]


