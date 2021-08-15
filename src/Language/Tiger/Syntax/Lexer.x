{

{-# LANGUAGE BlockArguments #-}

module Language.Tiger.Syntax.Lexer
  ( lex,
  )
where

import Prelude hiding (lex)
import Control.Monad
import Data.Char
import Language.Tiger.Syntax.Token qualified as Tok
import Language.Tiger.Syntax.Loc qualified as Loc

}

%wrapper "monadUserState"

$whitechar = [\ \t\n\r\f\v]

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

<0>                 $white+                       ;
<0>                 "/*"                          { enterComment `andBegin` state_comment }
<state_comment>     "/*"                          { enterComment }
<state_comment>     "*/"                          { leaveComment }
<state_comment>     .                             ;

<0>                 type                          { special Tok.Type }
<0>                 var                           { special Tok.Var }
<0>                 function                      { special Tok.Func }
<0>                 of                            { special Tok.Of }
<0>                 end                           { special Tok.End }
<0>                 in                            { special Tok.In }
<0>                 nil                           { special Tok.Nil }
<0>                 let                           { special Tok.Let }
<0>                 do                            { special Tok.Do }
<0>                 to                            { special Tok.To }
<0>                 for                           { special Tok.For }
<0>                 while                         { special Tok.While }
<0>                 else                          { special Tok.Else }
<0>                 then                          { special Tok.Then }
<0>                 if                            { special Tok.If }
<0>                 array                         { special Tok.Array }

<0>                 ":="                          { special Tok.Assign }
<0>                 "|"                           { special Tok.Or }
<0>                 "&"                           { special Tok.And }
<0>                 ">="                          { special Tok.Ge }
<0>                 ">"                           { special Tok.Gt }
<0>                 "<="                          { special Tok.Le }
<0>                 "<"                           { special Tok.Lt }
<0>                 "<>"                          { special Tok.Neq }
<0>                 "="                           { special Tok.Eq }
<0>                 "/"                           { special Tok.Div }
<0>                 "*"                           { special Tok.Mul }
<0>                 "-"                           { special Tok.Sub }
<0>                 "+"                           { special Tok.Add }
<0>                 "."                           { special Tok.Dot }
<0>                 "{"                           { special Tok.RBrace }
<0>                 "}"                           { special Tok.LBrace }
<0>                 "["                           { special Tok.RBrack }
<0>                 "]"                           { special Tok.LBrack }
<0>                 "("                           { special Tok.RParen }
<0>                 ")"                           { special Tok.LParen }
<0>                 ";"                           { special Tok.Semi }
<0>                 ":"                           { special Tok.Colon }
<0>                 ","                           { special Tok.Comma }

<0>                 @decimal                      { mkAction (Tok.Int . read) }

<0>                 \"                            { enterString `andBegin` state_string }
<state_string>      \"                            { leaveString `andBegin` 0 }
<state_string>      \\n                           { accumChar '\n' }
<state_string>      \\t                           { accumChar '\t' }
<state_string>      \\\\                          { accumChar '\\' }
<state_string>      \"                            { accumChar '\"' }
<state_string>      \\ $whitechar+ \\             ;
<state_string>      .                             { accumCurrent }
<state_string>      \n                            { skip }
<state_string>      \\ $digit{3}                  { accumAscii }

<0>                 @ident                        { mkAction Tok.Id }

{

mkAction f (AlexPn offset _ _, _, _, str) len =
  pure . Loc.At (Loc.Span offset (offset + len)) $ f (take len str)

special = mkAction . const

enterComment _ _ = do
  modifyCommentDepth (+ 1)
  alexMonadScan

leaveComment _ _ = do
  modifyCommentDepth (subtract 1)
  cd <- getCommentDepth
  when (cd == 0) do
    alexSetStartCode 0
  alexMonadScan

enterString _ _ = do
  setStringAcc ""
  alexMonadScan

accumChar c _ _ = modifyStringAcc (c :) *> alexMonadScan
accumCurrent i@(_, _, _, input) len = accumChar (head input) i len
accumAscii i@(_, _, _, input) len = accumChar (chr . read $ drop 1 input) i len

leaveString (AlexPn offset _ _, _, _, _) len = do
  s <- getStringAcc
  pure $ Loc.At (Loc.Span offset (offset + len)) (Tok.Str $ reverse s)

data AlexUserState = AlexUserState
  { commentDepth :: Int,
    stringAccum :: String
  }

alexInitUserState = AlexUserState
  { commentDepth = 0,
    stringAccum = ""
  }

getCommentDepth = Alex \s@AlexState { alex_ust = ust } ->
  Right (s, commentDepth ust)
setCommentDepth = modifyCommentDepth . const
modifyCommentDepth f = Alex \s@AlexState { alex_ust = ust } ->
  Right (s { alex_ust = ust { commentDepth = f (commentDepth ust) } }, ())

getStringAcc = Alex \s@AlexState { alex_ust = ust } ->
  Right (s, stringAccum ust)
setStringAcc = modifyStringAcc . const
modifyStringAcc f = Alex \s@AlexState { alex_ust = ust } ->
  Right (s { alex_ust = ust { stringAccum = f (stringAccum ust) } }, ())

alexEOF = do
  (AlexPn offset _ _, _, _, _) <- alexGetInput
  pure $ Loc.At (Loc.Span offset offset) Tok.EOF

lex s = runAlex s (loop [])
  where
    loop toks = do
      tok@(Loc.At _ kind) <- alexMonadScan
      let toks' = tok : toks
      if kind == Tok.EOF
        then pure $ reverse toks'
        else loop toks'

}
