
import System.Environment
import Data.Complex
import Paraiso


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
           r =$ Rand 0.0 (4.0::Double) 
           x =$ Rand 0.0 (1.0::Double) 
           cuda $ do
             sequential 65536 $ do
               x =$ r * x * (1-x)
           output [r,x]


