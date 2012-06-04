{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Language.Paraiso.Generator.OMTrans (
  translate
  ) where

import qualified Algebra.Additive                       as Additive
import qualified Data.Graph.Inductive                   as FGL
import           Data.Maybe (catMaybes)
import qualified Data.Set                               as Set
import           Data.Tensor.TypeLevel
import qualified Data.Vector                            as V
import qualified Language.Paraiso.Annotation            as Anot
import qualified Language.Paraiso.Annotation.Allocation as Alloc
import qualified Language.Paraiso.Annotation.Boundary   as Boundary
import qualified Language.Paraiso.Annotation.Dependency as Dep
import qualified Language.Paraiso.Generator.Plan        as Plan
import qualified Language.Paraiso.Generator.Native      as Native
import qualified Language.Paraiso.Interval              as Interval
import           Language.Paraiso.Name
import qualified Language.Paraiso.OM                    as OM
import qualified Language.Paraiso.OM.DynValue           as DVal
import qualified Language.Paraiso.OM.Graph              as OM
import qualified Language.Paraiso.OM.Realm              as Realm
import qualified Language.Paraiso.Optimization          as Opt
import qualified Language.Paraiso.PiSystem              as Pi
import           Language.Paraiso.Prelude
import           NumericPrelude hiding ((++))


data Triplet v g 
  = Triplet
  { tKernelIdx :: Int,
    tNodeIdx   :: FGL.Node,
    tNode      :: OM.Node v g Anot.Annotation
  } deriving (Show)

translate :: (Opt.Ready v g) =>
  Native.Setup v g -> OM.OM v g Anot.Annotation -> Plan.Plan v g Anot.Annotation
translate setup omBeforeOptimize = ret
  where
    ret = Plan.Plan 
      { Plan.planName   = name om,
        Plan.setup      = OM.setup om,
        Plan.kernels    = OM.kernels om,
        Plan.storages   = storages,
        Plan.subKernels = subKernels,
        Plan.lowerMargin = validToLower $ storageValidUnited om,  
        Plan.upperMargin = validToUpper $ storageValidUnited om  
      }

    om = Opt.optimize (Native.optLevel setup) omBeforeOptimize

    storages = staticRefs V.++ manifestRefs 
    subKernels = V.generate subKernelSize generateSubKernel

    staticRefs = 
      V.imap (\idx sVar -> 
               Plan.StorageRef { 
                 Plan.storageRefParent = ret,
                 Plan.storageIdx       = Plan.StaticRef idx,
                 Plan.storageDynValue  = namee sVar
               } ) $
      OM.staticValues $ OM.setup om

    manifestRefs = 
      V.map (\(Triplet kidx idx nd) -> 
              Plan.StorageRef {
                Plan.storageRefParent = ret,
                Plan.storageIdx       = Plan.ManifestRef kidx idx,
                Plan.storageDynValue  = getDynValue nd
              } ) $
      manifestNodes 

    manifestNodes = -- the vector of (kernelID, nodeID, node) of      
      V.concatMap findManifest $ -- elements marked as Manifest, found in
      V.concat . V.toList $ -- a monolithic vector of 
      V.imap (\kidx lns     -- triplet (kernelID, nodeID, node) built from
        -> V.fromList $ flip map lns (\(idx, n) -> Triplet kidx idx n)) $
      V.map FGL.labNodes $ -- the vector of lists of (idx, OM.Node) of
      V.map OM.dataflow  $ -- the graphs contained in
      OM.kernels om        -- the kernels of the om

    findManifest (Triplet kidx idx nd) = 
      case Anot.toMaybe $ OM.getA nd of
        Just Alloc.Manifest -> V.fromList [Triplet kidx idx nd]
        _                   -> V.empty

    omWriteGroup = 
      flip V.map manifestNodes $
        \ tri@(Triplet _ _ nd) -> case Anot.toMaybe $ OM.getA nd of
          Just omg@(Dep.OMWriteGroup _) -> omg
          Nothing -> error $ "OMWriteGroup missing : " ++ show tri

    getRealm (OM.NValue dv _) = DVal.realm dv
    getRealm _                = error $ "invalid request for Realm; probably a non-Value node is marked as Manifest"
    getDynValue (OM.NValue dv _) = dv
    getDynValue _                = error $ "invalid request for DVal; probably a non-Value node is marked as Manifest"



    validToLower valid = let Boundary.Valid xs = valid in
      compose $ \ax -> case Native.boundary setup ! ax of
        Boundary.Cyclic -> Additive.zero
        _ -> case Interval.lower (xs !! (axisIndex ax)) of
             Boundary.NegaInfinity    -> Additive.zero
             Boundary.LowerBoundary x -> x
             _                        -> error $ "wrong lower Margin! : " 
    validToUpper valid = negate $ let Boundary.Valid xs = valid in
      compose $ \ax -> case Native.boundary setup ! ax of
        Boundary.Cyclic -> Additive.zero
        _ -> case Interval.upper (xs !! (axisIndex ax)) of
            Boundary.PosiInfinity    -> Additive.zero
            Boundary.UpperBoundary x -> x
            _                        -> error "wrong upper Margin!"

    -- the intersection of the valid area of all the store instructions found in the om.
    storageValidUnited :: (Opt.Ready v g) =>
                       OM.OM v g Anot.Annotation -> Boundary.Valid g
    storageValidUnited _ = 
      foldl1 Pi.intersection $
      concat $
      map findStoreValid $
      concat . V.toList $ -- a flat list made from
      V.map (map snd . FGL.labNodes) $ -- the vector of lists of OM.Node of
      V.map OM.dataflow  $ -- the graphs contained in
      OM.kernels om        -- the kernels of the om
      where
        findStoreValid nd = case nd of
          OM.NValue _ _ -> []
          OM.NInst (OM.Store _) a -> Anot.toList a
          OM.NInst _ _ -> []


    subKernelSize = 
      (1 +) $ 
      maximum $ 
      (-1 :) $
      V.toList $
      V.map Dep.getOMGroupID $
      omWriteGroup


    generateSubKernel groupIdx = 
      mkSubKernel groupIdx $
      V.ifilter (\i _ -> omWriteGroup V.! i == Dep.OMWriteGroup groupIdx) $
      manifestNodes 

    mkSubKernel groupIdx myNodes =
      Plan.SubKernelRef 
      { Plan.subKernelParent = ret,
        Plan.kernelIdx       = (tKernelIdx $ myNodes V.! 0),
        Plan.omWriteGroupIdx = groupIdx,
        Plan.outputIdxs      = V.map tNodeIdx myNodes,
        Plan.inputIdxs       = inputs ,
        Plan.calcIdxs        = calcs,
        Plan.subKernelRealm  = skRealm,
        Plan.lowerBoundary   = validToLower $ skValid om,
        Plan.upperBoundary   = validToUpper $ skValid om
      }
      where
        inputs :: V.Vector FGL.Node
        inputs =
          V.fromList $ Set.toList $
          Set.unions $
          map (\(Dep.Direct xs) -> Set.fromList xs) $
          depDirects
        depDirects :: [Dep.Direct]
        depDirects = 
          catMaybes $
          V.toList $
          V.map (Anot.toMaybe . OM.getA . tNode) myNodes

        calcs :: V.Vector FGL.Node
        calcs = 
          V.fromList $ Set.toList $ 
          Set.unions $
          map (\(Dep.Calc xset) -> xset) $
          depCalcs
        depCalcs :: [Dep.Calc]
        depCalcs = 
          catMaybes $
          V.toList $
          V.map (Anot.toMaybe . OM.getA . tNode) myNodes

        skRealm :: Realm.Realm
        skRealm = 
          assertAllSame $
          map (getRealm . tNode) $
          V.toList myNodes

        skValid :: (Opt.Ready v g) => OM.OM v g Anot.Annotation -> Boundary.Valid g
        skValid _ =
          assertAllSame $
          concat $
          map (Anot.toList . OM.getA . tNode) $
          V.toList myNodes          

-- | check if all the elements are the same and returns it
assertAllSame :: Eq a => [a] -> a
assertAllSame xs = case xs of
  []     -> error "no elements!"
  (x:ys) -> if all (==x) ys then x
            else error "elements are different. mismatch."



{-

      -------------
    /               \       \\ *** Exception: Prelude.undefined //
  /     _,'      `._  \              ______
 /    ( (X) )  ( (X) ) \            | |-   - 
 |        (__/\__)      |           | | |   |
 \         |rt++|      /            | | |   |
  \        `----'     /             | | |  _| 
    '               '               | | | |  \
  /                    \            | | | |   |
 |    |                  \          | | | |   |
 \    -'''''''~       -'''''''~     |_|-  |   |
  \ ___(`)(`)`))     (`.(`)(`)`))        /_____\





                                                  /
         -------------                                .
       /  `._       __,'                        /    /
     /    ( (X))  ( (X) )\           ______         /
    /        (__/\__)     \         \ |-   -               _ -
    |         |rt++|       |         \| | |   |        _ - 
    \         `----'      /        __  | | |   |     
     \   _ -- -- -- -- -- -- --  /    \\\ | |  _|                 _ .
       '                .          ,_  ||| | | |  \       __  ---
      /      -- -- -- -- -- -- --.__ ..  | | |   |
     |                   \            | | |   |
                    -'''''''~        /_|-  |   |
                   (`.(`)(`)`))          /_____\









     .       '
             |      '
       \           /
    ----\----|--- /
  /      \   |   / \
-- __       ___      \               ______
/     --  /     \ - -- -- -- -- -- -| |-   - 
|        |  ,          ,            | | |   |
 \        \_ \_  ,--- -- -- -- -- --| | |   |
  \                   /             | | |  _| 
    '               '               | | | |  \
  /                    \            | | | |   |
 |    |                  \          | | | |   |
 \    -'''''''~       -'''''''~     |_|-  |   |
  \ ___(`)(`)`))     (`.(`)(`)`))        /_____\






-}