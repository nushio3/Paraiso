import Paraiso
import Data.Complex
import System.Environment


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
           z <- allocate
           z0<- allocate
           z =$ (Rand (-2.0) 2.0) :+ (Rand (-2.0) 2.0)
           z0=$ (z:: Complex (Expr Double))
           cuda $ do
             sequential 65536 $ do
               z =$ z * z - 1
           output [realPart z0,imagPart z0, realPart z, imagPart z]


