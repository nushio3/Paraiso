import Control.Monad
import VVM
import VVMCompiler

dag = DAG [
       (  0, Load (StaticRhs 0) (-1,-1)) ,
       (  1, Load (StaticRhs 0) (-1, 0)) ,
       (  2, Load (StaticRhs 0) (-1, 1)) ,
       (  3, Load (StaticRhs 0) ( 0,-1)) ,
       (  4, Load (StaticRhs 0) ( 0, 1)) ,
       (  5, Load (StaticRhs 0) ( 1,-1)) ,
       (  6, Load (StaticRhs 0) ( 1, 0)) ,
       (  7, Load (StaticRhs 0) ( 1, 1)) ,
       (  8, Add  0  1),
       (  9, Add  8  2),
       ( 10, Add  9  3),
       ( 11, Add 10  4),
       ( 12, Add 11  5),
       ( 13, Add 12  6),
       ( 14, Add 13  7),
       ( 15, Load (StaticRhs 0) ( 0, 0)) ,
       (100, Imm 0),
       (101, Imm 1),
       (102, Imm 2),
       (103, Imm 3),
       (200, IsEqual 15 100),
       (201, IsEqual 15 101),
       (202, IsEqual 14 102),
       (203, IsEqual 14 103),
       (300, And 200 203),
       (301, And 201 202),
       (302, And 201 203),
       (350, Or 300 301),
       (351, Or 350 302),
       (900, Select 351 101 100),
       (StaticLhs 0, Store (0,0) 900)
      ] 

machine = VVM {staticSize = 1}

glider = [(1,0), (2,1), (0,2), (1,2), (2,2)]
acorn = [(1,0), (3,1), (0,2), (1,2), (4,2), (5,2), (6,2)]

{-
  .0.       .0.....
  ..0       ...0...
  000       00..000
-} 

        

main = do
  let prob = ProblemConfiguration {
               duration = 64,
               extent = (64,32),
               initialConfiguration = acorn
             }
  putStr $ compile X86 prob machine   dag
