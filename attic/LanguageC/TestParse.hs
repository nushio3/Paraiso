#!/usr/bin/env runhaskell

import Data.ByteString as BS
import Language.C.Data.Position (initPos)
import Language.C.Parser
import Language.C.Pretty 

main = do
  input <- BS.getContents
  case execParser_ translUnitP input (initPos "STDIN") of
    Left err -> print err
    Right x -> print $ pretty x
