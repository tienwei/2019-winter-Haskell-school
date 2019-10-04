import Data.Tree
import Employee
import Party
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Party" $ do
      let gl1 = GL [Emp {empName = "Emp 1", empFun = 10}] 10
      let gl2 = GL [Emp {empName = "Emp 2", empFun = 15}] 15
      describe "exercie 1" $ do
        describe "1.1" $ do
          it "should return a correct guestList with an added Employee" $ do
            let initEL = [Emp {empName = "John", empFun = 8}]
            let initGL = GL initEL 8
            let newEmp = Emp {empName = "Tien", empFun = 15}
            glCons newEmp initGL `shouldBe` (GL (newEmp : initEL) 23)
        describe "1.2" $ do
          it "should do mconcat to add to GuestLists" $ do
            mconcat [gl1, gl2] `shouldBe`
              (GL
                 [ Emp {empName = "Emp 1", empFun = 10}
                 , Emp {empName = "Emp 2", empFun = 15}
                 ]
                 25)
        describe "1.3" $ do
          it "should return more fun guestList" $ do
            moreFun gl1 gl2 `shouldBe` gl2
      describe "exercie 2" $ do
        it "should work on the treeFold" $ do
          let mockTree =
                Node
                  { rootLabel = 1
                  , subForest =
                      [ Node {rootLabel = 5, subForest = []}
                      , Node {rootLabel = 3, subForest = []}
                      ]
                  }
          treeFold (\x y -> x + sum (y)) mockTree `shouldBe` 9
      describe "exercie 3" $ do
        it "should return a pair of optimal guestList with and without the boss" $ do
          let boss1 = Emp {empName = "boss1", empFun = 12}
          let boss2 = Emp {empName = "boss2", empFun = 20}
          let withBossGL1 = glCons boss1 gl1
          let withBossGL2 = glCons boss1 gl2
          let groupGLs = [(withBossGL1, gl1), (withBossGL2, gl2)]
          nextLevel boss2 groupGLs `shouldBe`
            ( GL [Emp {empName = "boss2", empFun = 20}] 20
            , GL
                [ Emp {empName = "boss2", empFun = 20}
                , Emp {empName = "Emp 2", empFun = 15}
                ]
                35)
      describe "exercie 4" $ do
        it "should return maxFun guest list from a company" $ do
          (getFun . maxFun $ testCompany) `shouldBe` 26
