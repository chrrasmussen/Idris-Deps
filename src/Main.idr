module Main

import System
import Control.Pipeline
import Text.Lexer
import public Text.Parser
import Control.Monad.State
import Data.SortedSet


-- LEXER

data IdrisTokenKind
  = IIgnore
  | ISymbol
  | IIdentifier

Show a => Show (Token a) where
  show (Tok x _) = show x

Show IdrisTokenKind where
  show IIgnore = " "
  show ISymbol = "symbol"
  show IIdentifier = "identifier"

Eq IdrisTokenKind where
  (==) IIgnore IIgnore = True
  (==) ISymbol ISymbol = True
  (==) IIdentifier IIdentifier = True
  (==) _ _ = False

TokenKind IdrisTokenKind where
  TokType IIgnore = ()
  TokType ISymbol = String
  TokType IIdentifier = String

  tokValue IIgnore x = ()
  tokValue ISymbol x = x
  tokValue IIdentifier x = x

IdrisToken : Type
IdrisToken = Token IdrisTokenKind

comment : Lexer
comment = is '-' <+> is '-' <+> many (isNot '\n')

commentAuxStyle : Lexer
commentAuxStyle = is '|' <+> is '|' <+> is '|' <+> many (isNot '\n')

toEndComment : (k : Nat) -> Recognise (k /= 0)
toEndComment Z = empty
toEndComment (S k) =
  some (pred (\c => c /= '-' && c /= '{')) <+> toEndComment (S k) <|>
    is '{' <+> is '-' <+> toEndComment (S (S k)) <|>
    is '-' <+> is '}' <+> toEndComment k <|>
    is '{' <+> toEndComment (S k) <|>
    is '-' <+> toEndComment (S k)

blockComment : Lexer
blockComment = is '{' <+> is '-' <+> toEndComment 1

ident : Lexer
ident = pred startIdent <+> many (pred validIdent)
  where
    startIdent : Char -> Bool
    startIdent '_' = True
    startIdent x = isAlpha x

    validIdent : Char -> Bool
    validIdent '_' = True
    validIdent '\'' = True
    validIdent x = isAlphaNum x

tokenMap : TokenMap IdrisToken
tokenMap = toTokenMap
  [ (comment, IIgnore)
  , (commentAuxStyle, IIgnore)
  , (blockComment, IIgnore)
  , (spaces, IIgnore)
  , (symbols, ISymbol)
  , (ident, IIdentifier)
  ]

runLexer : String -> List (TokenData IdrisToken)
runLexer str =
  let (res, _) = lex tokenMap str
  in res

ignored : IdrisToken -> Bool
ignored (Tok IIgnore _) = True
ignored _ = False


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
  mod : Maybe Module
  imports : List Import


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
      moduleString = case mod of
        Just mod' => show mod'
        _ => ""
      importString = unlines (map show imports)
    in
      moduleString ++ "\n\n" ++ importString


-- PARSER

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

namespace_ : Parser (List String)
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
  mod <- optional module_
  imports <- many import_
  pure (MkIdrisHead mod imports)

runParser : String -> Grammar IdrisToken e a -> Maybe a
runParser str parser =
  let
    tokens = map tok (runLexer str)
    validTokens = filter (not . ignored) $ tokens
    Right (res, _) = parse parser validTokens
      | Nothing
  in Just res


-- DIR/FILES

parseModule : String -> List String -> IO (Maybe IdrisHead)
parseModule rootDir ns = do
  let path = joinString "/" (rootDir :: ns) ++ ".idr"
  Right contents <- readFile path
    | pure Nothing
  pure (runParser contents program)

traverseModules : String -> List String -> StateT (SortedSet (List String)) IO ()
traverseModules rootDir ns' = do
  parsedModules <- get
  let False = contains ns' parsedModules
    | lift (putStrLn "*** Skipping")
  modify (insert ns')
  Just moduleRes <- lift (parseModule rootDir ns')
    | pure ()
  lift (print moduleRes)
  lift (putStrLn "---")
  traverse (traverseModules rootDir) (map ns (imports moduleRes))
  pure ()


-- MAIN

run : String -> String -> IO ()
run rootDir mainModule = do
  (_, ns') <- runStateT (traverseModules rootDir [mainModule]) empty
  putStrLn "All namespaces:"
  putStrLn $ unlines $ map showNamespace (SortedSet.toList ns')
  putStrLn "*** Finished"

partial
main : IO ()
main = do
  [_, rootDir, mainModule] <- getArgs
    | putStrLn "Usage: ./deps <rootDir> <mainModule>"
  run rootDir mainModule
