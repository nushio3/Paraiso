import Test.Framework (defaultMain)

-- Actual tests
import Test.Paraiso.Annotation
import Test.Paraiso.QuickCheckItself
import Test.Paraiso.Tensor

main :: IO ()
main = defaultMain [testQuickCheckItself, testAnnotation, testTensor]
