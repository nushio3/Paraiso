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
           c <- allocate
           z <- allocate 
           c =$ (Rand (-2.0) 2.0) :+ (Rand (-2.0) 2.0)
           z =$ (0 :+ 0 :: Complex (Expr Double))
           cuda $ do
             sequential (65536) $ do
               z =$ z * z + c
           output [realPart c,imagPart c, realPart z, imagPart z]


