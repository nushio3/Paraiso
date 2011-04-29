{-# LANGUAGE DeriveDataTypeable, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

import "mtl" Control.Monad.Trans
import Control.Monad.Exception
import Control.Monad.Loc

data MyException = MyException deriving (Show, Typeable)
instance Exception MyException

f () = do throw MyException
g a  = do f a

main = runEMT $ do g () `catchWithSrcLoc` (\loc (e::MyException) -> lift(putStrLn$ showExceptionWithTrace loc e))
