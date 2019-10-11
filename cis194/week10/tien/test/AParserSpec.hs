import AParser
import Data.Char
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "AParser" $ do
      describe "exercise 1" $ do
        it "should fmap Parser Char to Parser Integer" $ do
          runParser newParserInt testStr `shouldBe` (runParser posInt testStr)
  where
    f = toInteger . digitToInt
    newParserInt = fmap f (char '2')
    testStr = "2xyz"
