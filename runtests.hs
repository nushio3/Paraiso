import Test.Framework (defaultMain)

-- Actual tests
import Test.Annotation
import Test.QuickCheckItself
import Test.Tensor

main :: IO ()
main = defaultMain [testQuickCheckItself, testAnnotation, testTensor]
