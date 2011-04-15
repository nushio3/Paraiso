import NPP

input :: Expr
input = a*a + b*b + 0.5*c*c
    where  
      [a,b,c] = map Term $ words "a b c"

main :: IO ()
main = do
  print input
  putStrLn $ gen SSE2v2r8 $ optimize $ input

