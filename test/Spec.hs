import Test.Hspec
import Test.QuickCheck
import Parsing.YmlParser
import Parsing.Lexer
import Data.Either
import qualified Typing.BidiTyping as Bidi
import qualified Typing.TypeContext as Context
import qualified Typing.TypedTree as Typed
import qualified Parsing.AST as AST 
import System.IO
import qualified Pipeline as Pipeline
import System.Directory
import Parsing.ParserCombinator
import Debug.Trace
import Data.List

-- data PipelineTest = 
--   PipelineTest { 
--     testName :: String,
--     testTokens :: Maybe [Token],
--     testAST :: Maybe AST.Toplevel,
--     testTypedAST :: Maybe (Either Typed.Toplevel [String])
--   }
--   deriving(Eq)

-- instance Show PipelineTest where
--   show PipelineTest{testName = n, testTokens = t, testAST = ast, testTypedAST = tast } =
--     "pipeline test '" ++ n ++ "':" ++
--     "\n>> tokens:\n" ++ show t ++
--     "\n>> ast:\n" ++ show ast ++ 
--     "\n>> tast:\n" ++ show tast ++
--     "\n" 

-- readSource :: String -> IO String
-- readSource path = do
--     file <- openFile path ReadMode
--     hGetContents file


-- pipelineTest :: FilePath -> IO PipelineTest
-- pipelineTest path = do
--     source <- readSource path
--     return $ case tokenize source of 
--         Left err -> PipelineTest { 
--           testName = path, 
--           testTokens = Nothing, 
--           testAST = Nothing, 
--           testTypedAST = Nothing 
--         }
--         Right toks -> 
--           let 
--             ast = Parsing.YmlParser.parse toks
--             ctx = Context.toplevelTyping ast
--             tast = Bidi.checkToplevelType ctx ast
--           in PipelineTest {
--             testName = path,
--             testTokens = Just toks,
--             testAST = Just ast,
--             testTypedAST = Just tast
--           }

-- batchPipelineTest :: IO [PipelineTest]
-- batchPipelineTest = do 
--   let folder = "test/pipeline"
--   files <- listDirectory folder
--   let paths = filter (isSuffixOf ".l") (map (\x -> folder ++ "/" ++ x) files)
--   mapM pipelineTest paths

main :: IO ()
main = do
  -- pipelineTestResult <- batchPipelineTest
  hspec $ do
    describe "Parser:" $ do
        let process = (parse . fromRight [] . tokenize) :: Program
         
        
