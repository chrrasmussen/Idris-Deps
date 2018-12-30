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

data ModuleCache
  = AlreadyParsed (Tree (IdrisHead, Bool))
  | External

isAlreadyParsed : ModuleCache -> Bool
isAlreadyParsed (AlreadyParsed _) = True
isAlreadyParsed External = False

partial
traverseModules : String -> List String -> StateT (SortedMap (List String) ModuleCache) IO (Tree (IdrisHead, Bool))
traverseModules rootDir ns' = do
  let externalNode = Node (MkIdrisHead (MkModule ns') [], False) []
  parsedModules <- get
  let Nothing = SortedMap.lookup ns' parsedModules
    | Just (AlreadyParsed localNode) => pure localNode
    | Just External => pure externalNode
  Just currentIdrisHead <- lift (parseModule rootDir ns')
    | do
      modify (insert ns' External)
      pure externalNode
  subModules <- traverse (traverseModules rootDir) (map ns (imports currentIdrisHead))
  let node = Node (currentIdrisHead, True) subModules
  modify (insert ns' (AlreadyParsed node))
  pure node


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
    let (local, external) = partition (isAlreadyParsed . snd) allNs
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
