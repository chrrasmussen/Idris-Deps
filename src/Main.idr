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
getDependees : Namespace -> Tree (IdrisHead, Bool) -> SortedMap Namespace Bool
getDependees usesNs (Node (idrisHead, isLocal) subForest) =
  let
    allDepsList = map (toList . getDependees usesNs) subForest
    sortedDeps = fromList (join allDepsList)
    currentNs = ns (mod idrisHead)
  in
    if usesNs `elem` (map ns (imports idrisHead)) then
      SortedMap.insert currentNs isLocal sortedDeps
    else
      sortedDeps

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

moduleCacheToIsLocal : (Namespace, ModuleCache) -> (Namespace, Bool)
moduleCacheToIsLocal (ns', moduleCache) =
  (ns', isAlreadyParsed moduleCache)

showModule : (Namespace, Bool) -> String
showModule (ns', isLocal) =
  showNamespace ns' ++ showLocal isLocal

parseNamespace : String -> Maybe Namespace
parseNamespace str =
  runParser str namespace_

-- CLI

data ListOption
  = ListAll
  | ListLocal
  | ListExternal

partial
listModules : String -> Namespace -> ListOption -> IO ()
listModules rootDir mainNs listOption = do
  (_, ns') <- runStateT (traverseModules rootDir mainNs) empty
  let allNs = SortedMap.toList ns'
  let (local, external) = partition (isAlreadyParsed . snd) allNs
  let nsOutput =
    case listOption of
      ListAll => map (showModule . moduleCacheToIsLocal) allNs
      ListLocal => map (showNamespace . fst) local
      ListExternal => map (showNamespace . fst) external
  putStr (unlines nsOutput)


partial
depTree : String -> Namespace -> IO ()
depTree rootDir mainNs = do
    (tree, _) <- runStateT (traverseModules rootDir mainNs) empty
    let (treeSkippingModules, _) = runState (skipPreviousModules tree) empty
    let moduleNames = map showModuleFromIdrisHead treeSkippingModules
    putStr $ drawTree moduleNames
  where
      showModuleFromIdrisHead : (IdrisHead, Bool) -> String
      showModuleFromIdrisHead (idrisHead, isLocal) =
        showModule (ns (mod idrisHead), isLocal)

partial
usesDep : String -> Namespace -> Namespace -> IO ()
usesDep rootDir mainNs usesNs = do
  (tree, _) <- runStateT (traverseModules rootDir mainNs) empty
  let (treeSkippingModules, _) = runState (skipPreviousModules tree) empty
  let nsUsedIn = getDependees usesNs treeSkippingModules
  let allNs = SortedMap.toList nsUsedIn
  putStr $ unlines $ map showModule allNs

printUsage : IO ()
printUsage =
  putStrLn "Usage: ./deps <rootDir> <mainModule> [--list-all | --list-local | --list-external | --tree | --uses <module>]"

printInvalidNamespace : IO ()
printInvalidNamespace =
  putStrLn "Invalid namespace"

partial
main : IO ()
main = do
  (_ :: rootDir :: mainModule :: restArgs) <- getArgs
    | printUsage
  let Just mainNs = parseNamespace mainModule
    | printInvalidNamespace
  case restArgs of
    ["--list-all"] =>
      listModules rootDir mainNs ListAll

    ["--list-local"] =>
      listModules rootDir mainNs ListLocal

    ["--list-external"] =>
      listModules rootDir mainNs ListExternal

    ["--tree"] =>
      depTree rootDir mainNs

    ["--uses", usesModule] =>
      let Just usesNs = parseNamespace usesModule
        | printInvalidNamespace
      in usesDep rootDir mainNs usesNs

    _ =>
      printUsage
