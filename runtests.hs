import Test.Framework (defaultMain)

-- Actual tests
import Test.Annotation
import Test.QuickCheckItself


main :: IO ()
main = defaultMain [testQuickCheckItself, testAnnotation]
