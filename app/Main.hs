import Data.List

import Control.Monad
import Language.Haskell.Interpreter
import System.Directory

main :: IO ()
main = do setCurrentDirectory "examples"
          r <- runInterpreter testHint
          case r of
            Left err -> putStrLn $ errorString err
            Right () -> return ()

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

say :: String -> Interpreter ()
say = liftIO . putStrLn

emptyLine :: Interpreter ()
emptyLine = say ""

-- observe that Interpreter () is an alias for InterpreterT IO ()
testHint :: Interpreter ()
testHint =
    do
      say "Load SomeModule.hs"
      loadModules ["SomeModule.hs"]
      setTopLevelModules ["SomeModule"]
      setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M")]
      let expr1 = "length $ concat [[f,g],[h]]"
      a_int <- interpret expr1 (as :: Int)
      say $ show a_int
      let expr2 = "i 10"
      b_bool <- interpret expr2 (as :: Bool)
      say $ show b_bool
      emptyLine
