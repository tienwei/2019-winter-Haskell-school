import Data.Monoid
import Test.Hspec

import  JoinList
import Sized

main :: IO ()
main =
  hspec $ do
    describe "JoinList" $ do
      describe "exercise 1" $ do
        it "should return correct JoinList" $ do
          let jl1 = Single (Product 5) 'e'
          let jl2 =
                Append
                  (Product 30)
                  (Single (Product 10) 'g')
                  (Single (Product 3) 'h')
          jl1 +++ jl2 `shouldBe` (Append (Product 150) jl1 jl2)

      describe "exercise 2" $ do
        it "should return Nothing if the index not found" $ do
          let jl1 = Append (Size 4) (Append (Size 3) (Single (Size 1) 'a') (Append (Size 2) (Single (Size 1) 'b') (Single (Size 1) 'c'))) (Single (Size 1) 'd')
          let jl2 = Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'f')
          let jl3 = Append (Size 6) jl1 jl2
          indexJ 3 jl3 `shouldBe` (jlToList jl3 !!? 3)
