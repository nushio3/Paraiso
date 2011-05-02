{-# OPTIONS -Wall #-}
-- | a generic code generator definition.
module Language.Paraiso.Generator
    (Generator(..)) where
import Language.Paraiso.POM

class Generator gen where
    generate :: gen -> POM v g a -> FilePath -> IO ()

