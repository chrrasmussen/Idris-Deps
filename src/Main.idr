module Main

import System
import Control.Monad.State
import Data.SortedMap
import Data.SortedSet

import Data.Tree
import Deps.Data
import Deps.Lexer
import Deps.Parser


%default total


-- DIR/FILES

parseModule : String -> Namespace -> IO (Maybe IdrisHead)
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
traverseModules : String -> Namespace -> StateT (SortedMap Namespace ModuleCache) IO (Tree (IdrisHead, Bool))
traverseModules rootDir ns' = do
  let externalNode = Node (MkIdrisHead (MkModule ns') [], False) []
  parsedModules <- get
  let Nothing = SortedMap.lookup ns' parsedModules
    | Just (AlreadyParsed localNode) => pure localNode
    | Just External => pure externalNode
  Just parsedIdrisHead <- lift (parseModule rootDir ns')
    | do
      modify (insert ns' External)
      pure externalNode
  subModules <- traverse (traverseModules rootDir) (map ns (imports parsedIdrisHead))
  let node = Node (record { mod = MkModule ns' } parsedIdrisHead, True) subModules
  modify (insert ns' (AlreadyParsed node))
  pure node

partial
skipPreviousModules : Tree (IdrisHead, Bool) -> State (SortedSet Namespace) (Tree (IdrisHead, Bool))
skipPreviousModules (Node (idrisHead, isLocal) subForest) = do
  let currentNs = ns (mod idrisHead)
  parsedModules <- get
  let False = SortedSet.contains currentNs parsedModules
    | pure (Node (record { imports = [] } idrisHead, isLocal) [])
  modify (SortedSet.insert currentNs)
  updatedSubForest <- traverse skipPreviousModules subForest
  pure (Node (idrisHead, isLocal) updatedSubForest)

-- UTILS

showLocal : Bool -> String
showLocal True = ""
showLocal False = " (Lib)"

showModule : (IdrisHead, Bool) -> String
showModule (idrisHead, isLocal) =
  showNamespace (ns (mod idrisHead)) ++ showLocal isLocal


-- CLI

partial
listModules : String -> String -> IO ()
listModules rootDir mainModule = do
  (_, ns') <- runStateT (traverseModules rootDir [mainModule]) empty
  let allNs = SortedMap.toList ns'
  let (local, external) = partition (isAlreadyParsed . snd) allNs
  putStrLn "*** All local modules"
  putStrLn $ unlines $ map (showNamespace . fst) local
  putStrLn "*** All external modules"
  putStrLn $ unlines $ map (showNamespace . fst) external

partial
depTree : String -> String -> IO ()
depTree rootDir mainModule = do
  (tree, _) <- runStateT (traverseModules rootDir [mainModule]) empty
  let (treeSkippingModules, _) = runState (skipPreviousModules tree) empty
  let moduleNames = map showModule treeSkippingModules
  putStrLn "*** Dependency tree"
  putStrLn $ drawTree moduleNames

partial
main : IO ()
main = do
  args <- getArgs
  case args of
    [_, rootDir, mainModule, "--ls"] =>
      listModules rootDir mainModule

    [_, rootDir, mainModule, "--tree"] =>
      depTree rootDir mainModule

    _ =>
      putStrLn "Usage: ./deps <rootDir> <mainModule> [--ls | --tree]"
