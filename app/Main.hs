import Lalo.Syntax (Expr)
import Lalo.Eval (runEval)
import Lalo.Parser (parse)

import Error.Diagnose

import Data.Either

import Control.Monad.Trans
import System.Console.Haskeline
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU

process :: B.ByteString -> IO ()
process input = do
  case parse input of
      Left err -> do
        putStrLn "Parse Error:"
        -- let diag = addFile def "some_test.txt" "let id<a>(x : a) : a := x\n  + 1"
        -- let diag' = addReport diag err
        -- printDiagnostic stdout False True 4 defaultStyle diag'
        putStrLn $ show err
      Right ast -> exec ast

exec :: Expr -> IO ()
exec ast = do
  let result = runEval ast
  case result of
    Left err -> do
      putStrLn "Runtime Error:"
      -- let diag = addFile def "some_test.txt" "let id<a>(x : a) : a := x\n  + 1"
      -- let diag' = addReport diag err
      -- printDiagnostic stdout False True 4 defaultStyle diag'
      putStrLn $ show err
    Right res -> print res

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Happy> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process $ BSU.fromString input) >> loop
