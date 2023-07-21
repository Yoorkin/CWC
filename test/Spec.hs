import Test.Hspec
import Test.QuickCheck
import Parsing.YmlParser
import Parsing.Lexer
import Data.Either
import Parsing.AST
import qualified Typing.BidiTyping as Bidi
import qualified Typing.TypeContext as Context
import Parsing.AST (Toplevel(Toplevel))

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        let process = parse . fromRight [] . tokenize

        it "test unhappy parser" $ 
         let result = process "data Tree a = Leaf of a | Node of (Tree a, Tree a)"
         in result `shouldBe` Toplevel [] []
       
        it "test record declaration" $
         let result = process "data CStyleRecord a = CStyleRecord of { leftChild : Tree a, rightChild : Tree b, count : Int }"
         in result `shouldBe` Toplevel [] []

        let result = process "fib : int -> int = fun x -> if x <= 2 then x else let r = fib (x - 1) + fib (x - 2) in r"
       
        it "test toplevel binding" $
          result `shouldBe` Toplevel [] []

        let ctx = Context.toplevelTyping result
        let tast = Bidi.checkToplevelType ctx result

        it "test bidi-check of toplevel binding" $
           tast `shouldBe` Right []

     

