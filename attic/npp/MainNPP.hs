import NPP

input :: Expr
input = a + b * c + 0.5 * a
    where  
      [a,b,c] = map Term $ words "a b c"

main :: IO ()
main = do
  print input
  putStrLn $ gen input

