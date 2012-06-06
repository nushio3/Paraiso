{-# OPTIONS -Wall #-}

-- | Paraiso main module.
-- This module will export a starter-kit modules and functions in the future,
-- but is useless right now.
-- The building-blocks of Paraiso programs are in 
-- the module 'Language.Paraiso.OM.Builder.Builder'.
--
-- A series of intoduction is being written in Japanese
-- <http://d.hatena.ne.jp/nushio/searchdiary?word=*%5BICFDP%5D>
-- and English
-- <http://nushisblogger.blogspot.jp/search/label/ICFDP>.
--
-- Also refer to the wiki
-- <http://www.paraiso-lang.org/wiki/> and the paper
-- <http://arxiv.org/abs/1204.4779> for more detail.



module Language.Paraiso (run) where

-- | Generate Wonderful Program
run :: () -> String
run _ = "#include <iostream>\nint main () {cout << \"hello\" << endl;}\n"
