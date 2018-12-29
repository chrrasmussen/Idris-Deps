module Deps.Lexer

import public Text.Lexer


%default total
%access public export


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
