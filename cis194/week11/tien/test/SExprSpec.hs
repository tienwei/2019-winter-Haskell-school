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
      describe "exercise 2" $ do
        it "should return correct results of spaces" $ do
          runParser spaces "  test123" `shouldBe` Just ("  ", "test123")
          runParser spaces "test123" `shouldBe` Just ("", "test123")
        it "should return correct results of ident" $ do
          runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
          runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
          runParser ident "2bad" `shouldBe` Nothing
          runParser ident "" `shouldBe` Nothing
