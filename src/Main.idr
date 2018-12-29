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
  let fakeModule = MkModule ns'
  parsedModules <- get
  let Nothing = SortedMap.lookup ns' parsedModules
    | Just isLocal => pure (Node (MkIdrisHead fakeModule [], isLocal) [])
  Just currentIdrisHead <- lift (parseModule rootDir ns')
    | do
      modify (insert ns' False)
      pure (Node (MkIdrisHead fakeModule [], False) [])
  modify (insert ns' True)
  subModules <- traverse (traverseModules rootDir) (map ns (imports currentIdrisHead))
  pure (Node (currentIdrisHead, True) subModules)

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
    let moduleNames = map showModule tree
    putStrLn "*** Dependency tree"
    putStrLn $ drawTree moduleNames

    let allNs = SortedMap.toList ns'
    let (local, external) = partition ((== True) . snd) allNs
    putStrLn "*** All local modules"
    putStrLn $ unlines $ map (showNamespace . fst) local
    putStrLn "*** All external modules"
    putStrLn $ unlines $ map (showNamespace . fst) external
  where
    showLocal : Bool -> String
    showLocal True = ""
    showLocal False = " (Lib)"

    showModule : (IdrisHead, Bool) -> String
    showModule (idrisHead, isLocal) =
      showNamespace (ns (mod idrisHead)) ++ showLocal isLocal

partial
main : IO ()
main = do
  [_, rootDir, mainModule] <- getArgs
    | putStrLn "Usage: ./deps <rootDir> <mainModule>"
  run rootDir mainModule
