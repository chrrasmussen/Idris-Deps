module Main


%default total
%access public export


-- UTILS

joinString : String -> List String -> String
joinString sep str =
  concat (intersperse sep str)

showNamespace : List String -> String
showNamespace ns =
  joinString "." ns


-- DATA TYPES

record Module where
  constructor MkModule
  ns : List String

record Import where
  constructor MkImport
  isPublic : Bool
  ns : List String
  nsAs : List String

record IdrisHead where
  constructor MkIdrisHead
  mod : Module
  imports : List Import


defaultModule : Module
defaultModule =
  MkModule ["Main"]


-- SHOW IMPLEMENTATIONS

Show Module where
  show (MkModule ns) = "module " ++ showNamespace ns

Show Import where
  show (MkImport isPublic ns nsAs) =
    let
      publicSpecifier = if isPublic then "public " else ""
      nsAsSpecifier = if ns /= nsAs then " as " ++ showNamespace nsAs else ""
    in
      "import " ++ publicSpecifier ++ showNamespace ns ++ nsAsSpecifier

Show IdrisHead where
  show (MkIdrisHead mod imports) =
    let
      moduleString = show mod
      importString = unlines (map show imports)
    in
      moduleString ++ "\n\n" ++ importString
