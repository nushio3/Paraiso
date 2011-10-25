import           Control.Exception (finally)
import           Control.Monad
import           Test.Framework (defaultMainWithArgs)
import qualified Test.Paraiso.Option as Option

-- Actual tests
import Test.Paraiso.Annotation
import Test.Paraiso.Claris 
import Test.Paraiso.QuickCheckItself

main :: IO ()
main = 
  flip defaultMainWithArgs Option.argv 
    [testQuickCheckItself, 
     testAnnotation, 
     testClaris]
    `finally` when Option.help Option.printHelp
