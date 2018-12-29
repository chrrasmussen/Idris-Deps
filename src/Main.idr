module Main

import System
import Control.Monad.State
import Data.SortedMap

import Data.Tree
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
traverseModules : String -> List String -> StateT (SortedMap (List String) Bool) IO (Tree (IdrisHead, Bool))
traverseModules rootDir ns' = do
  parsedModules <- get
  let Nothing = SortedMap.lookup ns' parsedModules
    | Just isLocal => pure (Node (MkIdrisHead defaultModule [], isLocal) [])
  Just currentModule <- lift (parseModule rootDir ns')
    | do
      modify (insert ns' False)
      pure (Node (MkIdrisHead defaultModule [], False) [])
  modify (insert ns' True)
  subModules <- traverse (traverseModules rootDir) (map ns (imports currentModule))
  pure (Node (currentModule, True) subModules)

partial
printModules : Tree IdrisHead -> IO ()
printModules (Node rootLabel subForest) = do
  print rootLabel
  putStrLn "---"
  traverse printModules subForest
  pure ()


-- MAIN

partial
run : String -> String -> IO ()
run rootDir mainModule = do
  (tree, ns') <- runStateT (traverseModules rootDir [mainModule]) empty
  printModules (map fst tree)
  let moduleNames = map (showNamespace . ns . IdrisHead.mod . fst) tree
  putStrLn $ drawTree moduleNames
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
