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


-- HELPER FUNCTIONS

filterMap : (a -> Maybe b) -> List a -> List b
filterMap pred xs =
  foldl
    (\acc, current => case pred current of
      Just result => result :: acc
      Nothing => acc)
    []
    xs


-- DIR/FILES

parseModule : String -> Namespace -> IO (Maybe IdrisHead)
parseModule rootDir ns = do
  let path = joinString "/" (rootDir :: ns) ++ ".idr"
  Right contents <- readFile path
    | pure Nothing
  pure (runParser contents program)

data ModuleCache
  = AlreadyParsed (Tree (IdrisHead, Bool))
  | Skipped

isAlreadyParsed : ModuleCache -> Bool
isAlreadyParsed (AlreadyParsed _) = True
isAlreadyParsed Skipped = False

data ParsedModule
  = Local IdrisHead
  | External

isLocalModule : ParsedModule -> Bool
isLocalModule (Local _) = True
isLocalModule External = False

partial
parseModulesHelper : String -> Namespace -> StateT (SortedMap Namespace ParsedModule) IO ()
parseModulesHelper rootDir ns' = do
  parsedModules <- get
  let Nothing = SortedMap.lookup ns' parsedModules
    | pure () -- Skip already parsed modules
  Just parsedModule <- lift (parseModule rootDir ns')
    | modify (insert ns' External)
  let localModule = Local (record { moduleNs = ns' } parsedModule)
  modify (insert ns' localModule)
  traverse (parseModulesHelper rootDir) (map ns (imports parsedModule))
  pure ()

partial
parseModules : String -> Namespace -> IO (SortedMap Namespace ParsedModule)
parseModules rootDir ns' = do
  (_, state) <- runStateT (parseModulesHelper rootDir ns') empty
  pure state

partial
buildTree : SortedMap Namespace ParsedModule -> Namespace -> State (SortedMap Namespace ModuleCache) (Tree (IdrisHead, Bool))
buildTree parsedModules ns' = do
  let externalNode = Node (MkIdrisHead ns' [], False) []
  trees <- get
  let Nothing = SortedMap.lookup ns' trees
    | Just (AlreadyParsed localNode) => pure localNode
    | Just Skipped => pure externalNode
  let Just (Local parsedIdrisHead) = SortedMap.lookup ns' parsedModules
    | do
      modify (insert ns' Skipped)
      pure externalNode
  subModules <- traverse (buildTree parsedModules) (map ns (imports parsedIdrisHead))
  let node = Node (record { moduleNs = ns' } parsedIdrisHead, True) subModules
  modify (insert ns' (AlreadyParsed node))
  pure node

partial
traverseModules : String -> Namespace -> IO (Tree (IdrisHead, Bool))
traverseModules rootDir ns' = do
  parsedModules <- parseModules rootDir ns'
  let (tree, _) = runState (buildTree parsedModules ns') empty
  pure tree

partial
getDependees : Namespace -> List IdrisHead -> List Namespace
getDependees usesNs idrisHeads =
  filterMap
    (\idrisHead => if usesNs `elem` map ns (imports idrisHead)
      then Just (moduleNs idrisHead)
      else Nothing)
    idrisHeads

partial
skipPreviousModules : Tree (IdrisHead, Bool) -> State (SortedSet Namespace) (Tree (IdrisHead, Bool))
skipPreviousModules (Node (idrisHead, isLocal) subForest) = do
  let currentNs = moduleNs idrisHead
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

parsedModuleToIsLocal : (Namespace, ParsedModule) -> (Namespace, Bool)
parsedModuleToIsLocal (ns', moduleCache) =
  (ns', isLocalModule moduleCache)

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
  modules <- parseModules rootDir mainNs
  let allNs = SortedMap.toList modules
  let (local, external) = partition (isLocalModule . snd) allNs
  let nsOutput =
    case listOption of
      ListAll => map (showModule . parsedModuleToIsLocal) allNs
      ListLocal => map (showNamespace . fst) local
      ListExternal => map (showNamespace . fst) external
  putStr (unlines nsOutput)


partial
depTree : String -> Namespace -> IO ()
depTree rootDir mainNs = do
    tree <- traverseModules rootDir mainNs
    let (treeSkippingModules, _) = runState (skipPreviousModules tree) empty
    let moduleNames = map showModuleFromIdrisHead treeSkippingModules
    putStr $ drawTree moduleNames
  where
      showModuleFromIdrisHead : (IdrisHead, Bool) -> String
      showModuleFromIdrisHead (idrisHead, isLocal) =
        showModule (moduleNs idrisHead, isLocal)

partial
usesDep : String -> Namespace -> Namespace -> IO ()
usesDep rootDir mainNs usesNs = do
  parsedModules <- parseModules rootDir mainNs
  let usedInNs = getDependees usesNs (localModules parsedModules)
  putStr $ unlines $ map showNamespace usedInNs
where
  localModules : SortedMap Namespace ParsedModule -> List IdrisHead
  localModules parsedModules =
    filterMap
      (\parsedModule => case parsedModule of
        Local idrisHead => Just idrisHead
        External => Nothing)
      (values parsedModules)

printUsage : IO ()
printUsage =
  putStrLn "Usage: deps <rootDir> <mainModule> [--list-all | --list-local | --list-external | --tree | --uses <module>]"

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
