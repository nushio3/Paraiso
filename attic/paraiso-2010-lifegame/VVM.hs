-- Virtual Vector Machine, a virtual machine that describes cellular automata.

module VVM (
            VVM(..),
            DAG(..),
            Instruction(..),
            Register(..),
            Coordinate, ValueType,
            conformal
           ) where

type Coordinate = (Int, Int)
type ValueType = Int

{-
  Registers of VVM is either Static (passed to The Next Generation)
  or Local (destructed at the end of generation.)
  Registers are nodes of the dataflow DAG (directed acyclic graph.)
 -}

data Register = StaticRhs Int | Local Int | StaticLhs Int  deriving (Eq, Ord, Show, Read)


isStatic :: Register -> Bool
isStatic (StaticLhs _) = True
isStatic (StaticRhs _) = True
isStatic _ = False

isStaticLhs :: Register -> Bool
isStaticLhs (StaticLhs _) = True
isStaticLhs _ = False

isStaticRhs :: Register -> Bool
isStaticRhs (StaticRhs _) = True
isStaticRhs _ = False

isLocal :: Register -> Bool
isLocal (Local _) = True
isLocal _ = False



instance Num Register where
  fromInteger = Local . fromInteger
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined


{-
  The instruction set for the Cell Machine.
  Here are the semantices.
  
  Load a s i     : d = s[i]
  Store s i a    : s[i] = a
  Add d a b      : d = a + b
  Sub d a b      : d = a - b
  Imm d val      : d = val
  And d a b      : d = a && b
  Or  d a b      : d = a || b
  IsEqual d a b  : d = a == b
  Select d a b c : d = a ? b : c
-}

data Instruction 
   = Load  Register Coordinate
   | Store Coordinate Register
   | Add Register Register
   | Sub Register Register
   | Imm ValueType
   | And Register Register
   | Or Register Register
   | IsEqual Register Register
   | Select Register Register Register
     deriving (Eq, Ord, Show, Read)

srcRegisters :: Instruction -> [Register]
srcRegisters (Load a _) = [a]
srcRegisters (Store _ a) = [a]
srcRegisters (Add a b) = [a, b]
srcRegisters (Sub a b) = [a, b]
srcRegisters (Imm _) = []
srcRegisters (And a b) = [a, b]
srcRegisters (Or a b) = [a, b]
srcRegisters (IsEqual a b) = [a, b]
srcRegisters (Select a b c) = [a, b, c]


{-
  Specifications for VVM
 -}
data VVM = VVM { staticSize :: Int }
    
{-
  Programs for VVM are DAG that starts from StaticRHS nodes
  end ends in StaticLHS nodes.
 -}
data DAG = DAG {nodes  :: [(Register, Instruction)]}
           deriving (Show)


    
{-
  The conditions that VVM programs must satisfy are:
  
  * ascending : the local nodes are indexed in ascending order
  * acyclic : the destination register has index larger than source registers.
  (above two are sufficient conditions for acyclicity)
  * epimorphism : all the static registers are written once and only once 
  * inRange : source and destination registers exist
  * correctDestination : each instruction uses correct type of register
 -}
conformal :: VVM -> DAG -> Bool
conformal machine dag@DAG{nodes = nodes} = and
    [ascending (map fst nodes) ,
     all acyclic nodes ,
     epimorphism ,
     all inRange (map fst nodes) ,
     (all inRange . concat . map (srcRegisters.snd)) nodes ,
     all correctDestination nodes]
    
    where
      ascending xs = and $  zipWith (<) xs (tail xs)
      
      acyclic (destReg, inst) = 
          all (\srcReg -> srcReg < destReg) 
                  (filter isLocal $ srcRegisters inst)
      
      staticLhsRegisters = [ StaticLhs i | i <- [0..staticSize machine - 1]]
      staticRhsRegisters = [ StaticRhs i | i <- [0..staticSize machine - 1]]
      localRegisters = filter isLocal $ map fst nodes

      writeInstToStatics = [ filter ((==sr) . fst) nodes 
                             | sr <- staticLhsRegisters]

      epimorphism = all ((==1) . length) writeInstToStatics
      
      inRange reg = case reg of
        StaticLhs _ -> elem reg staticLhsRegisters
        StaticRhs _ -> elem reg staticRhsRegisters
        Local _ -> elem reg localRegisters
      
      correctDestination (dreg, inst) = let sregs = srcRegisters inst in
          case inst of
            (Store _ _) -> isStaticLhs dreg && all isLocal sregs 
            (Load _ _) -> isLocal dreg && all isStaticRhs sregs 
            _ -> isLocal dreg && all isLocal sregs
            