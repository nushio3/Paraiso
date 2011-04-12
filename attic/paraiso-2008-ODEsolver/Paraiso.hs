{-# OPTIONS -O3 -optc-O3 -XFlexibleInstances #-}

module Paraiso(
  Architecture(..),
  Dsl,
  Expr(..),
  Register(..),(=$),
  compile,
  parallel,
  sequential,
  cuda,
  mesh
)where

import Control.Monad.RWS
import Data.Complex
import Data.List
import Data.Maybe


import Util
import Debug





type VarID = String
type VarModifier = String -> String
modRegister::VarModifier
modRegister = (++ "_reg")
modDevice::VarModifier
modDevice = (++ "_dev")
flagRegister = ""  --"__shared__ "


data Architecture = X86 | 
                    CUDA{numberOfGrids::Int, numberOfThreads::Int} |
                    MPI Architecture deriving Eq

data Statement = Statement{funcName::String, phase::StatementType}

data StatementType = Alloc String | Store String | Free String |            
              FuncCall String [String] [String] 
              deriving (Eq, Ord, Show, Read)
funcNameMain = "main"

data HardwareContext = Host | Device String deriving (Eq, Ord, Show, Read)
data Context = Context 
    {
     precision::String,
     freeVariable::VarID, storeModifier::VarModifier, allocModifier::VarModifier,
     parallelDegree::Int,
     architecture:: Architecture,
     hardware::HardwareContext,
     fogArgument::[String],
     fogArgumentType::[String],
     openHostParallel::[String],
     closeHostParallel::[String],
     memSendHD::[String],
     memRecvHD::[String],
     memSendDH::[String],
     memRecvDH::[String]
    } 


data Expr a = 
    Imm a |
    Rand a a | 
    Var VarID |
    Add (Expr a) (Expr a) |
    Sub (Expr a) (Expr a) |
    Mul (Expr a) (Expr a) |
    Div (Expr a) (Expr a) |
    Abs (Expr a) |
    Rec (Expr a) |
    Sgn (Expr a) deriving (Eq, Ord, Show, Read)

compileExpr:: (Show a) => VarModifier -> Expr a -> String
compileExpr mo = c where
    c (Imm a) = show a
    c (Rand lo hi) = "drand(" ++ show lo ++ " , " ++ show hi ++ ")"
    c (Var v) = mo v 
    c (Add a b) = "(" ++ c a ++ " + " ++ c b ++ ")"
    c (Sub a b) = "(" ++ c a ++ " - " ++ c b ++ ")"
    c (Mul a b) = "(" ++ c a ++ " * " ++ c b ++ ")"
    c (Div a b) = "(" ++ c a ++ " / " ++ c b ++ ")"
    c (Abs a) = "abs(" ++ c a ++ ")"
    c (Rec a) = "(1.0 / " ++ c a ++ ")"
    c (Sgn a) = "(" ++ c a ++ " >0?1:-1)"


instance (Num t) => Num (Expr t) where
    a + b = Add a b
    a - b = Sub a b
    a * b = Mul a b
    abs a = Abs a
    signum a = Sgn a
    fromInteger i =  Imm (fromInteger i)

instance Fractional t => Fractional (Expr t) where
    a / b = Div a b
    recip a = Rec a
    fromRational i = Imm (fromRational i)

instance Real t => Real (Expr t) where
    toRational = undefined

instance Floating t => Floating (Expr t) where
    pi = undefined
    exp = undefined
    log = undefined
    sin = undefined
    cos = undefined
    asin = undefined
    atan = undefined
    acos = undefined
    sinh = undefined
    cosh = undefined
    asinh = undefined
    atanh = undefined
    acosh = undefined

instance RealFloat t => RealFloat (Expr t) where
    floatRadix = undefined
    floatDigits = undefined
    floatRange = undefined
    decodeFloat = undefined
    encodeFloat = undefined
    isNaN = undefined
    isInfinite = undefined
    isDenormalized = undefined
    isNegativeZero = undefined
    isIEEE = undefined

instance RealFrac t => RealFrac (Expr t) where
    properFraction = undefined

type Dsl a = RWS () [Statement] Context a



tellA::[String] -> Dsl ()
tellA xs = do
  s <- get
  case hardware s of
    Host -> tellHA xs
    Device fn -> tellDA fn xs
tellS::[String] -> Dsl ()
tellS xs = do
  s <- get
  case hardware s of
    Host -> tellHS xs
    Device fn -> tellDS fn xs

tellHA :: [String] -> Dsl ()
tellHA = tell.map (\st -> Statement funcNameMain (Alloc st))
tellHS :: [String] -> Dsl ()
tellHS = tell.map (\st -> Statement funcNameMain (Store st))
tellHF :: [String] -> Dsl ()
tellHF = tell.map (\st -> Statement funcNameMain (Free st))
tellDA :: String -> [String] -> Dsl ()
tellDA fn = tell.map (\st -> Statement fn (Free st))
tellDS :: String -> [String] -> Dsl ()
tellDS fn = tell.map (\st -> Statement fn (Free st))

getVarName = do
  let inc (h:ts) = (h:(show.(1+).read $ ts))
  state <- get
  let v = freeVariable state
  put state{freeVariable = inc v}
  return v


class Register a where
    allocate :: Dsl a
    store::a -> a -> Dsl ()
    output::[a] -> Dsl ()    

(=$) :: Register a => a -> a -> Dsl ()
(=$) = store
infix 0 =$






instance Register (Expr Double) where
    allocate = do
      v <- getVarName
      s <- get
      let 
        am = allocModifier s
        sm = storeModifier s
        prec = precision s
        arch = architecture s
        parD = parallelDegree s
      if hardware s == Host 
         then do
           case arch of
             X86 -> tellA [am v  ++";"]
             CUDA{} -> do               
               let 
                 vDev = modDevice v
                 vSha  = modRegister v
                 sos = "sizeof(" ++ prec ++ ")*" ++ show parD
               tellHA  [prec ++ " *" ++ v ++ " = (" ++prec ++ "*) malloc(" ++sos++ ");"]
               tellHA  [prec ++ " *" ++ vDev ++ ";"]
               tellHA  ["cudaMalloc((void**) &" ++ vDev ++" ," ++ sos ++ ");"]
               s<-get
               put s{
                 memSendHD = (memSendHD s +:) $ 
                   "cudaMemcpy(" ++ vDev ++ " , " ++ v ++ ","
                      ++ sos ++ " , cudaMemcpyHostToDevice);",
                 memRecvHD = (memRecvHD s ++) $ 
                   [prec ++ " "++ flagRegister ++" " ++ vSha ++ ";" , vSha ++ " = " ++ sm vDev ++ ";"],
                 memSendDH = (memSendDH s +:) $ 
                   sm vDev ++ " = " ++ vSha ++ ";", 
                 memRecvDH = (memRecvDH s +:) $ 
                   "cudaMemcpy(" ++ v ++ " , " ++ vDev ++ ","
                     ++ sos ++ ", cudaMemcpyDeviceToHost);", 
                 fogArgument     = (fogArgument     s +:) $ vDev,
                 fogArgumentType = (fogArgumentType s +:) $ prec ++  " *" ++  vDev
               }
         else do
           tellA [prec ++ " " ++  am v ++";"]
      return $ Var v
    store addr expr = do
      case addr of
        Var v -> do
                  s <- get
                  let sm = storeModifier s
                  tellS [sm v ++ " = " ++ compileExpr sm expr ++ ";"]
        _ -> error "lhs is not a variable address."
    output addrs = do
      s <- get
      let 
          sm = storeModifier s
          fromVar ex = case ex of
                       Var id -> sm id 
                       _      -> "output is not a variable address."
          statement = 
              unwords $
              (++ ["<< endl;"] ) $
              (["cout <<"] ++ ) $
              intersperse ("<< \" \" <<") $
              map fromVar addrs
      tellS [statement]      


     

instance (RealFloat a, Register a) => Register (Complex a) where
    allocate = do
      r <- allocate
      i <- allocate
      return (r:+i)
    store (tr:+ti) (sr:+si) = do
      tmp <- allocate
      store tmp sr
      store ti si
      store tr tmp
    output = undefined


compile :: Architecture -> Dsl a-> String
compile arch a = code
  where
      (_,s,w) = runRWS a () initContext

      initContext = Context
            {
             freeVariable = "a1",
             storeModifier = id,
             allocModifier = ((precision initContext ++ " ") ++),
             parallelDegree = 1,
             architecture = arch, 
             hardware = Host,
             precision = case arch of
                           CUDA{} -> "float"
                           _      -> "double",
             fogArgument =[],
             fogArgumentType =[],
             openHostParallel = [],
             closeHostParallel = [],
             memSendHD = [],
             memRecvHD = [],
             memSendDH = [],
             memRecvDH = []
            }

          
      funcAndBodies = groupSort funcName $ w
      funcCalls::[StatementType]
      funcCalls = concatMap (\st -> case st of
                                   (Statement _ fc@FuncCall{}) -> [fc]
                                   _ -> []) w
      lookupFunc fn = case find (\(FuncCall n _ _) -> n==fn) funcCalls of
                 Just fc -> fc
                 Nothing -> error $ "function disappeared! " ++ fn


      code = unlines $ progHeader ++ concatMap buildFunc funcAndBodies
             
      buildFunc (fn, stmts) = let
          (header,footer) = sandwich fn
          mysort = sortBy (\a b->f a `compare` f b) 
          f (Statement _ Alloc{})    = 1
          f (Statement _ Store{})    = 2
          f (Statement _ FuncCall{}) = 2
          f (Statement _ Free{})     = 3
        in header ++ (map peal $ mysort stmts) ++ footer


      peal (Statement _ (Alloc s)) = s
      peal (Statement _ (Store s)) = s
      peal (Statement _ (Free  s)) = s
      peal (Statement _ (FuncCall fn args argtypes)) 
          = fn ++ "<<<grids,threads>>>(" ++ (unwords . intersperse ",") args ++ ");"

      progHeader = includes ++ utilfuncs
      includes = 
        (++["using namespace std;"]) $
        map (\fn -> "#include <" ++ fn ++ ">") $
        headers
      headers = ["iostream", "cstdlib"] 
        ++ case arch of
             CUDA{} -> [] -- ["cutil.h"]
             _      -> []
      utilfuncs = 
        [
         "double drand(double lo, double hi){",
         "return lo + rand()/(double)RAND_MAX * (hi-lo);",
         "}"
        ]

      sandwich fn = if fn == funcNameMain then
           (["int main(int argc, char **argv){"]
            ++ case arch of
              CUDA{numberOfGrids = ng,  numberOfThreads = nt}
                -> ["dim3 grids(" ++ show ng ++");",
                         "dim3 threads(" ++ show nt ++");"]
              _ -> []

           , ["return 0;","}"])
        else
            (["__global__ void " ++ fn ++ "(" ++ (unwords $ intersperse "," $ att fn) ++ "){"]
            , ["}"])
        
      --argumentTypeTable
      att fn = let (FuncCall _ _ fat) = lookupFunc fn in fat



       

parallel size dsl = do
  iv <- getVarName
  state <- get
  put $ state{
              storeModifier = (++ ("[" ++ iv ++ "]")) . storeModifier state,
              allocModifier = (++ ("[" ++ show size ++ "]")) . allocModifier state,
              memRecvHD = (:memRecvHD state) $ "int " ++ iv ++ " = blockIdx.x * gridDim.x + threadIdx.x;"
--              fogArgument = (:fogArgument state) $  iv, NEEDED_FOR_GREATER_LOOP
--              fogArgumentType = (:fogArgumentType state) $ "int " ++ iv
             }
  let
    open  = "for(int "++iv++" = 0 ; "++iv++" < " ++ show size ++ " ; ++"++iv++"){"
    close = "}"
  tellS [open]
  do
    state <- get
    put $ state{
            openHostParallel  = openHostParallel state ++ [open],
            closeHostParallel = [close] ++ closeHostParallel state,
            parallelDegree = size * parallelDegree state
          } 
    dsl
  tellS [close]
  put state
  return ()


mesh::[Int] -> Dsl () -> Dsl ()
mesh dimension dsl = do
  state <- get
  if architecture state /= X86 then 
      error "hey we just don't support mesh on GPU right now"
    else do
          ivs <- mapM (\ _ -> getVarName) dimension 
          let 
              shell f = concat . map (\str -> "[" ++ f str ++ "]")
              is = [0..(length ivs-1)]
              opens = ["for(int "++ivs!!i++" = 0 ; "
                       ++ivs!!i++" < " ++ show (dimension!!i) ++ " ; "
                       ++"++"++ivs!!i++"){" | i <- is]
              closes = ["}" | i<-is]

          state <- get
          tellS opens
          put $ state {
                       storeModifier = (++ shell id ivs) . storeModifier state,
                       allocModifier = (++ shell show dimension) . allocModifier state
                      }
          dsl
          tellS closes
          put state
  return ()



sequential size dsl = do
  iv <- getVarName
  tellS ["for(int "++iv++" = 0 ; "++iv++" < " ++ show size ++ " ; ++"++iv++"){"]
  dsl
  tellS ["}"]
  return ()

cuda dsl = do
  s <- get
  case architecture s of
    CUDA{} -> if hardware s == Host then
                  docuda s
              else
                  error "you can't use cuda inside cuda"
    _      -> dsl 
  where
    docuda s = do
      fnvar <- getVarName
      let fn = "function_on_GPU_" ++ fnvar
      tellS $ closeHostParallel s
      tellS $ memSendHD s
      tellDS fn $ memRecvHD s
      tell $ [Statement funcNameMain $ FuncCall fn (fogArgument s) (fogArgumentType s)]
      do
        put s{hardware = Device fn, storeModifier = modRegister, allocModifier = (flagRegister ++).modRegister}
        dsl 
      put s
      tellDS fn $ memSendDH s
      tellS $ memRecvDH s
      tellS $ openHostParallel s
