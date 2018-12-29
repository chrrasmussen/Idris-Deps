module Main

import System
import Control.Monad.State
import Data.SortedMap

import Deps.Data
import Deps.Lexer
import Deps.Parser


%default total


-- DIR/FILES

parseModule : String -> List String -> IO (Maybe IdrisHead)
parseModule rootDir ns = do
  let path = joinString "/" (rootDir :: ns) ++ ".idr"
  Right contents <- readFile path
    | pure Nothing
  pure (runParser contents program)

partial
traverseModules : String -> List String -> StateT (SortedMap (List String) Bool) IO ()
traverseModules rootDir ns' = do
  parsedModules <- get
  let Nothing = SortedMap.lookup ns' parsedModules
    | lift (putStrLn "*** Skipping")
  Just moduleRes <- lift (parseModule rootDir ns')
    | do
      modify (insert ns' False)
      pure ()
  modify (insert ns' True)
  lift (print moduleRes)
  lift (putStrLn "---")
  traverse (traverseModules rootDir) (map ns (imports moduleRes))
  pure ()


-- MAIN

partial
run : String -> String -> IO ()
run rootDir mainModule = do
  (_, ns') <- runStateT (traverseModules rootDir [mainModule]) empty
  let allNs = SortedMap.toList ns'
  let (local, external) = partition ((== True) . snd) allNs
  putStrLn "All local namespaces:"
  putStrLn $ unlines $ map (showNamespace . fst) local
  putStrLn "All external namespaces:"
  putStrLn $ unlines $ map (showNamespace . fst) external
  putStrLn "*** Finished"

partial
main : IO ()
main = do
  [_, rootDir, mainModule] <- getArgs
    | putStrLn "Usage: ./deps <rootDir> <mainModule>"
  run rootDir mainModule
