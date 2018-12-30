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
  let allDepsList = map (toList . getDependees usesNs) subForest
  in let sortedDeps : SortedMap Namespace Bool = fromList (join allDepsList)
  in if usesNs `elem` (map ns (imports idrisHead)) then
    let currentNs = ns (mod idrisHead)
    in SortedMap.insert currentNs isLocal sortedDeps
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

readNamespace : String -> Namespace
readNamespace str =
  split (== '.') str


-- CLI

data ListOption
  = ListAll
  | ListLocal
  | ListExternal

partial
listModules : String -> String -> ListOption -> IO ()
listModules rootDir mainModule listOption = do
  (_, ns') <- runStateT (traverseModules rootDir [mainModule]) empty
  let allNs = SortedMap.toList ns'
  let (local, external) = partition (isAlreadyParsed . snd) allNs
  case listOption of
    ListAll =>
      putStrLn $ unlines $ map (showModule . moduleCacheToIsLocal) allNs

    ListLocal =>
      putStrLn $ unlines $ map (showNamespace . fst) local

    ListExternal =>
      putStrLn $ unlines $ map (showNamespace . fst) external


partial
depTree : String -> String -> IO ()
depTree rootDir mainModule = do
    (tree, _) <- runStateT (traverseModules rootDir [mainModule]) empty
    let (treeSkippingModules, _) = runState (skipPreviousModules tree) empty
    let moduleNames = map showModuleFromIdrisHead treeSkippingModules
    putStrLn $ drawTree moduleNames
  where
      showModuleFromIdrisHead : (IdrisHead, Bool) -> String
      showModuleFromIdrisHead (idrisHead, isLocal) =
        showModule (ns (mod idrisHead), isLocal)

partial
usesDep : String -> String -> String -> IO ()
usesDep rootDir mainModule usesModule = do
  let usesNs = readNamespace usesModule
  (tree, _) <- runStateT (traverseModules rootDir [mainModule]) empty
  let nsUsedIn = getDependees usesNs tree
  let allNs = SortedMap.toList nsUsedIn
  putStrLn $ unlines $ map showModule allNs

partial
main : IO ()
main = do
  args <- getArgs
  case args of
    [_, rootDir, mainModule, "--list-all"] =>
      listModules rootDir mainModule ListAll

    [_, rootDir, mainModule, "--list-local"] =>
      listModules rootDir mainModule ListLocal

    [_, rootDir, mainModule, "--list-external"] =>
      listModules rootDir mainModule ListExternal

    [_, rootDir, mainModule, "--tree"] =>
      depTree rootDir mainModule

    [_, rootDir, mainModule, "--uses", usesModule] =>
      usesDep rootDir mainModule usesModule

    _ =>
      putStrLn "Usage: ./deps <rootDir> <mainModule> [--ls | --tree | --uses <module>]"
