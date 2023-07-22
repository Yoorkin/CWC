import Test.Hspec
import Test.QuickCheck
import Parsing.YmlParser
import Parsing.Lexer
import Data.Either
import Parsing.AST
import qualified Typing.BidiTyping as Bidi
import qualified Typing.TypeContext as Context
import Parsing.AST (Toplevel(Toplevel))
import System.IO
import qualified Pipeline as Pipeline
import System.Directory
import Parsing.ParserCombinator
import Debug.Trace
import Data.List

data TestFiles a = TestFiles [(String, a)] deriving(Eq)

instance Show a => Show (TestFiles a) where
  show (TestFiles xs) = let f (path,x) = "test file: " ++ path ++ ":\n" ++ show x ++ "\n" 
            in if null xs then "" else foldl1 (++) (map f xs)
readSource :: String -> IO String
readSource path = do
    file <- openFile path ReadMode
    hGetContents file

lexerTest :: IO (TestFiles (Either (Error Char) [Token]))
lexerTest = do
  let folder = "test/parser"
  files <- listDirectory folder
  let paths = map (\x -> folder ++ "/" ++ x) files
  let filtered = filter (isSuffixOf ".l") paths
  sources <- mapM readSource paths
  let results = map tokenize sources
  return $ TestFiles (zip filtered results)

parserTest :: IO (TestFiles Toplevel)
parserTest = do 
  let folder = "test/parser"
  files <- listDirectory folder
  let paths = map (\x -> folder ++ "/" ++ x) files
  let filtered = filter (isSuffixOf ".l") paths
  sources <- mapM readSource paths
  let tokens = map tokenize sources
  let process = \toks -> case toks of 
        Left err -> Toplevel [] []
        Right toks' -> Parsing.YmlParser.parse toks'
  return $ TestFiles (zip filtered (map process tokens))



main :: IO ()
main = do
  lexerTestResult <- lexerTest
  parserTestResult <- parserTest
  hspec $ do
    describe "Parser:" $ do
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

        it "test lexer which inputs from 'test/lexer/*.l'" $
           lexerTestResult `shouldBe` TestFiles []

        it "test parser which inputs from 'test/parser/*.l'" $
           parserTestResult `shouldBe` TestFiles []

     

