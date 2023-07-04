import Test.Hspec
import Test.QuickCheck
import Parsing.YmlParser
import Parsing.Lexer
import Data.Either
import Parsing.AST

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        let process = parse . fromRight [] . tokenize

        it "test unhappy parser" $ 
         let result = process "data Tree a = Leaf of a | Node of (Tree a, Tree a)"
         in result `shouldBe` [ToplevelType "Tree" ["a"] 
                                       [("Node",TypeTuple [TypeApply (TypeVar "Tree") (TypeVar "a"),TypeApply (TypeVar "Tree") (TypeVar "a")]),
                                       ("Leaf",TypeVar "a")]]
       
        it "test record declaration" $
         let result = process "data CStyleRecord a = CStyleRecord of { leftChild : Tree a, rightChild : Tree b, count : Int }"
         in result `shouldBe` []

        it "test toplevel binding" $
         let result = process "let rec fib x = if x <= 2 then x else fib (x - 1) + fib (x - 2)"
         in result `shouldBe` []