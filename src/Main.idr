module Main

import Control.Pipeline
import Text.Lexer
import public Text.Parser


-- LEXER

data IdrisTokenKind
  = IIgnore
  | IIdentifier

Show a => Show (Token a) where
  show (Tok x _) = show x

Show IdrisTokenKind where
  show IIgnore = " "
  show IIdentifier = "identifier"

Eq IdrisTokenKind where
  (==) IIgnore IIgnore = True
  (==) IIdentifier IIdentifier = True
  (==) _ _ = False

TokenKind IdrisTokenKind where
  TokType IIgnore = ()
  TokType IIdentifier = String

  tokValue IIgnore x = ()
  tokValue IIdentifier x = x

IdrisToken : Type
IdrisToken = Token IdrisTokenKind

ident : Lexer
ident =
  any <+> manyUntil spaces any

tokenMap : TokenMap IdrisToken
tokenMap = toTokenMap
  [ (spaces, IIgnore)
  , (ident, IIdentifier)
  ]

runLexer : String -> List (TokenData IdrisToken)
runLexer str =
  let (res, _) = lex tokenMap str
  in res

ignored : IdrisToken -> Bool
ignored (Tok IIgnore _) = True
ignored _ = False


-- PARSER

Parser : Type -> Type
Parser a = Grammar IdrisToken True a

data ModuleDecl = MkModule String

Show ModuleDecl where
  show (MkModule name) = "module " ++ name

data ImportDecl = MkImport Bool String

Show ImportDecl where
  show (MkImport isPublic name) = "import " ++ showPublic isPublic ++ name
    where
      showPublic : Bool -> String
      showPublic True = "public "
      showPublic False = ""


exactIdent : String -> Parser ()
exactIdent expectedName = do
  identName <- match IIdentifier
  if identName == expectedName
    then pure ()
    else fail ("Expected: " ++ expectedName)

moduleDecl : Parser ModuleDecl
moduleDecl = do
  exactIdent "module"
  name <- match IIdentifier
  pure (MkModule name)

importDecl : Parser ImportDecl
importDecl = do
  exactIdent "import"
  publicSpecifier <- optional (exactIdent "public")
  name <- match IIdentifier
  let isPublic = maybe False (const True) publicSpecifier
  pure (MkImport isPublic name)

program : Grammar IdrisToken False (Maybe ModuleDecl, List ImportDecl)
program = do
  mod <- optional moduleDecl
  imports <- many importDecl
  pure (mod, imports)

runParser : String -> Grammar IdrisToken e a -> Maybe a
runParser str parser =
  let
    tokens = map tok (runLexer str)
    validTokens = filter (not . ignored) $ tokens
    Right (res, _) = parse parser validTokens
      | Nothing
  in Just res


-- MAIN

main : IO ()
main = do
  Right contents <- readFile "Main.idr"
    | putStrLn "Read failed"
  let Just moduleName = runParser contents program
    | putStrLn "Parsing failed"
  printLn moduleName
  putStrLn "Finished"
