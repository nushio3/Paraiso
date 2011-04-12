module Debug(
             trace, watch, spy
)where

import qualified Debug.Trace as D

trace = D.trace
spy a b = trace (show a) b
watch a = trace (show a) a



