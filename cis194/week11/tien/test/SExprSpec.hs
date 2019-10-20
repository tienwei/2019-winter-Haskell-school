import AParser
import Data.Char
import SExpr
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "SExpr" $ do
      describe "exercise 1" $ do
        it "should return correct results of zeroOrMore" $ do
          runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`
            Just ("ABC", "dEfgH")
          runParser (zeroOrMore (satisfy isUpper)) "aBCdEfgH" `shouldBe`
            Just ("", "aBCdEfgH")
        it "should return correct results of oneOrMore" $ do
          runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`
            Just ("ABC", "dEfgH")
          runParser (oneOrMore (satisfy isUpper)) "aBCdEfgH" `shouldBe` Nothing
