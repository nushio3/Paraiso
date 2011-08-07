{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Sample (
  helloWorld
  ) where

import Claris as C

helloWorld :: C.Program
helloWorld = 
  C.Program {
    progName = "hello",
    topLevel = [PragmaDecl $ PragmaInclude "iostream" False Chevron]
    }