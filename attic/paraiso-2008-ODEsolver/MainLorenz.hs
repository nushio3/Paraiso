import Paraiso
import Integrator
import System.Environment
import Debug.Trace

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
          y <- allocate
          z <- allocate
          x =$ Rand 10 (20.0::Double)
          y =$ Rand 10 20
          z =$ Rand 10 20
          cuda $ do
             p <- allocate
             r <- allocate
             b <- allocate
             p =$ 10
             r =$ 28
             b =$ 8/3
             integrate4 0.01 (10 * 655.36) $ [
                    d_dt x $ - p*x + p*y     ,
                    d_dt y $ - x*z + r*x - y ,
                    d_dt z $   x*y - b*z      ] 
          output [x,y,z]

