module Deps.Data


%default total
%access public export


-- UTILS

joinString : String -> List String -> String
joinString sep str =
  concat (intersperse sep str)

Namespace : Type
Namespace = List String

showNamespace : Namespace -> String
showNamespace ns =
  joinString "." ns


-- DATA TYPES

record Import where
  constructor MkImport
  isPublic : Bool
  ns : Namespace
  nsAs : Namespace

record IdrisHead where
  constructor MkIdrisHead
  moduleNs : Namespace
  imports : List Import


defaultModuleNs : Namespace
defaultModuleNs =
  ["Main"]


-- SHOW IMPLEMENTATIONS

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
