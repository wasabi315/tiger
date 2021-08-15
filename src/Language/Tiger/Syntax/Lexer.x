{

module Language.Tiger.Syntax.Lexer
  ( lex,
  )
where

import Prelude hiding (lex)
import Data.Bifunctor
import Data.Char (chr)
import Language.Tiger.Syntax.Token qualified as Tok
import Language.Tiger.Syntax.Loc qualified as Loc

}

%wrapper "monad"

$whitechar = [ \t\n\r\f\v]

$digit = 0-9
$alpha = [a-zA-Z]

@ident = $alpha [$alpha $digit \_]*

@decimal = $digit+

@ctrl    = NUL | SOH | STX | ETX | EOT | ENQ | ACK
	       | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	       | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	       | SUB | ESC | FS | GS | RS | US | SP | DEL
@ascii   = $digit{3}
$charesc = [nt\\\"]
@escape  = \\ ($charesc | @ctrl | @ascii)
@gap     = \\ $whitechar+ \\
@strchar = $printable # [\"\\] | @escape | @gap
@string  = \" @strchar* \"

tokens :-

  $white+                       { skip }
  "/*"                          { skipComment }
  type                          { special Tok.Type }
  var                           { special Tok.Var }
  function                      { special Tok.Func }
  of                            { special Tok.Of }
  end                           { special Tok.End }
  in                            { special Tok.In }
  nil                           { special Tok.Nil }
  let                           { special Tok.Let }
  do                            { special Tok.Do }
  to                            { special Tok.To }
  for                           { special Tok.For }
  while                         { special Tok.While }
  else                          { special Tok.Else }
  then                          { special Tok.Then }
  if                            { special Tok.If }
  array                         { special Tok.Array }
  ":="                          { special Tok.Assign }
  "|"                           { special Tok.Or }
  "&"                           { special Tok.And }
  ">="                          { special Tok.Ge }
  ">"                           { special Tok.Gt }
  "<="                          { special Tok.Le }
  "<"                           { special Tok.Lt }
  "<>"                          { special Tok.Neq }
  "="                           { special Tok.Eq }
  "/"                           { special Tok.Div }
  "*"                           { special Tok.Mul }
  "-"                           { special Tok.Sub }
  "+"                           { special Tok.Add }
  "."                           { special Tok.Dot }
  "{"                           { special Tok.RBrace }
  "}"                           { special Tok.LBrace }
  "["                           { special Tok.RBrack }
  "]"                           { special Tok.LBrack }
  "("                           { special Tok.RParen }
  ")"                           { special Tok.LParen }
  ";"                           { special Tok.Semi }
  ":"                           { special Tok.Colon }
  ","                           { special Tok.Comma }
  @decimal                      { mkAction (Tok.Int . read) }
  @string                       { mkAction Tok.Str }
  @ident                        { mkAction Tok.Id }

{

mkAction :: (String -> a) -> AlexAction (Loc.Located a)
mkAction f (AlexPn offset _ _, _, _, str) len =
  pure . Loc.At (Loc.Span offset (offset + len)) $ f (take len str)

special :: Tok.Token -> AlexAction (Loc.Located Tok.Token)
special = mkAction . const

skipComment :: AlexAction (Loc.Located Tok.Token)
skipComment _ _ = alexGetInput >>= loop 1
  where
    loop :: Int -> AlexInput -> Alex (Loc.Located Tok.Token)
    loop 0 input = do
      alexSetInput input
      alexMonadScan
    loop n input = do
      case alexGetChar input of
        Nothing -> err input
        Just ('*', input') ->
          case alexGetChar input' of
            Nothing -> err input'
            Just ('/', input'') -> loop (n - 1) input''
            Just (_, input'') -> loop n input''
        Just ('/', input') ->
          case alexGetChar input' of
            Nothing -> err input'
            Just ('*', input'') -> loop (n + 1) input''
            Just (_, input'') -> loop n input''
        Just (_, input') ->
          loop n input'

    err input = do
      alexSetInput input
      alexError "error in nested comment"

alexEOF :: Alex (Loc.Located Tok.Token)
alexEOF = do
  (AlexPn offset _ _, _, _, _) <- alexGetInput
  pure $ Loc.At (Loc.Span offset offset) Tok.EOF

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar input = first (chr . fromIntegral) <$> alexGetByte input

lex :: String -> Either String [Loc.Located Tok.Token]
lex s = runAlex s (loop [])
  where
    loop toks = do
      tok@(Loc.At _ kind) <- alexMonadScan
      let toks' = tok : toks
      if kind == Tok.EOF
        then pure $ reverse toks'
        else loop toks'

}
