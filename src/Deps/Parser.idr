module Deps.Parser

import public Text.Parser

import Deps.Data
import Deps.Lexer


%default total
%access public export


Parser : Type -> Type
Parser a = Grammar IdrisToken True a

symbol : String -> Parser ()
symbol expectedName = do
  symbolName <- match ISymbol
  if symbolName == expectedName
    then pure ()
    else fail ("Expected: " ++ expectedName)

exactIdent : String -> Parser ()
exactIdent expectedName = do
  identName <- match IIdentifier
  if identName == expectedName
    then pure ()
    else fail ("Expected: " ++ expectedName)

namespace_ : Parser Namespace
namespace_ = do
  ns <- sepBy1 (symbol ".") (match IIdentifier)
  pure ns

module_ : Parser Module
module_ = do
  exactIdent "module"
  ns <- namespace_
  pure (MkModule ns)

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
  mod <- option defaultModule module_
  imports <- many import_
  pure (MkIdrisHead mod imports)

runParser : String -> Grammar IdrisToken e a -> Maybe a
runParser str parser =
  let
    tokens = map tok (runLexer str)
    validTokens = filter (not . ignored) tokens
    Right (res, _) = parse parser validTokens
      | Nothing
  in Just res
