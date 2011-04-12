import Paraiso
import Integrator
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
          x <- allocate
          p <- allocate
          x =$ Rand 0.1 (1.0::Double)
          cuda $ do
             integrate4 0.01 2.56 $ [
                    d_dt x $ p*x
                         ]
          output [x]

