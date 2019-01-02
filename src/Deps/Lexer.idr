module Deps.Lexer

-- Implementation heavily inspired by Idris' Language.JSON and Blodwen


import public Text.Lexer


%default total
%access public export


data IdrisTokenKind
  = Ignore
  | Symbol
  | Identifier

Show a => Show (Token a) where
  show (Tok x _) = show x

Show IdrisTokenKind where
  show Ignore = " "
  show Symbol = "symbol"
  show Identifier = "identifier"

Eq IdrisTokenKind where
  (==) Ignore Ignore = True
  (==) Symbol Symbol = True
  (==) Identifier Identifier = True
  (==) _ _ = False

TokenKind IdrisTokenKind where
  TokType Ignore = ()
  TokType Symbol = String
  TokType Identifier = String

  tokValue Ignore x = ()
  tokValue Symbol x = x
  tokValue Identifier x = x

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
  [ (comment, Ignore)
  , (commentAuxStyle, Ignore)
  , (blockComment, Ignore)
  , (spaces, Ignore)
  , (symbols, Symbol)
  , (ident, Identifier)
  ]

runLexer : String -> List (TokenData IdrisToken)
runLexer str =
  let (res, _) = lex tokenMap str
  in res

ignored : IdrisToken -> Bool
ignored (Tok Ignore _) = True
ignored _ = False
