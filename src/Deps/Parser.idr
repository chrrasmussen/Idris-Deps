module Deps.Parser

-- Implementation heavily inspired by Idris' Language.JSON and Blodwen


import public Text.Parser

import Deps.Data
import Deps.Lexer


%default total
%access public export


Parser : Type -> Type
Parser a = Grammar IdrisToken True a

symbol : String -> Parser ()
symbol expectedName = do
  symbolName <- match Symbol
  if symbolName == expectedName
    then pure ()
    else fail ("Expected: " ++ expectedName)

exactIdent : String -> Parser ()
exactIdent expectedName = do
  identName <- match Identifier
  if identName == expectedName
    then pure ()
    else fail ("Expected: " ++ expectedName)

namespace_ : Parser Namespace
namespace_ = do
  ns <- sepBy1 (symbol ".") (match Identifier)
  pure ns

module_ : Parser Namespace
module_ = do
  exactIdent "module"
  namespace_

import_ : Parser Import
import_ = do
  exactIdent "import"
  reexp <- option False (do
    exactIdent "public"
    pure True)
  ns <- namespace_
  nsAs <- option ns (do
    exactIdent "as"
    namespace_)
  pure (MkImport reexp ns nsAs)

program : Grammar IdrisToken False IdrisHead
program = do
  moduleNs <- option defaultModuleNs module_
  imports <- many import_
  pure (MkIdrisHead moduleNs imports)

runParser : String -> Grammar IdrisToken e a -> Maybe a
runParser str parser =
  let
    tokens = map tok (runLexer str)
    validTokens = filter (not . ignored) tokens
    Right (res, _) = parse parser validTokens
      | Nothing
  in Just res
