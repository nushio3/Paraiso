{-# LANGUAGE QuasiQuotes #-}

import JSON

main :: IO ()
main = do
  let twelve = 12::Int; name = "mr_konn"
  print $ [js| {ident: `twelve, name: `name} |]
  print $ [js| {ident: `twelve, name `name } |] -- cause a parse error.
