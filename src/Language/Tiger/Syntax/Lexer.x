{

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Language.Tiger.Syntax.Lexer
  ( lex,
  )
where

import Prelude hiding (lex)
import Control.Monad
import Data.Char (chr)
import Language.Tiger.Syntax.Token qualified as Tok
import Language.Tiger.Syntax.Loc qualified as Loc

}

%wrapper "monadUserState"

$whitechar = [\ \t\n\r\f\v]

$digit = 0-9
$alpha = [a-zA-Z]

@ident = $alpha [$alpha $digit \_]*

@decimal = $digit+

tokens :-

<0>                $white+              { skip }
<0>                "/*"                 { enterComment `andBegin` state_comment }
<state_comment>    "/*"                 { enterComment }
<state_comment>    "*/"                 { leaveComment }
<0>                "*/"                 { \_ _ -> alexError "Illegal comment close" }
<state_comment>    .                    { skip }
<state_comment>    \n                   { skip }

<0>                "type"               { located $ special Tok.Type }
<0>                "var"                { located $ special Tok.Var }
<0>                "function"           { located $ special Tok.Func }
<0>                "of"                 { located $ special Tok.Of }
<0>                "end"                { located $ special Tok.End }
<0>                "in"                 { located $ special Tok.In }
<0>                "nil"                { located $ special Tok.Nil }
<0>                "let"                { located $ special Tok.Let }
<0>                "do"                 { located $ special Tok.Do }
<0>                "to"                 { located $ special Tok.To }
<0>                "for"                { located $ special Tok.For }
<0>                "while"              { located $ special Tok.While }
<0>                "else"               { located $ special Tok.Else }
<0>                "then"               { located $ special Tok.Then }
<0>                "if"                 { located $ special Tok.If }
<0>                "array"              { located $ special Tok.Array }

<0>                ":="                 { located $ special Tok.Assign }
<0>                "|"                  { located $ special Tok.Or }
<0>                "&"                  { located $ special Tok.And }
<0>                ">="                 { located $ special Tok.Ge }
<0>                ">"                  { located $ special Tok.Gt }
<0>                "<="                 { located $ special Tok.Le }
<0>                "<"                  { located $ special Tok.Lt }
<0>                "<>"                 { located $ special Tok.Neq }
<0>                "="                  { located $ special Tok.Eq }
<0>                "/"                  { located $ special Tok.Div }
<0>                "*"                  { located $ special Tok.Mul }
<0>                "-"                  { located $ special Tok.Sub }
<0>                "+"                  { located $ special Tok.Add }
<0>                "."                  { located $ special Tok.Dot }
<0>                "{"                  { located $ special Tok.RBrace }
<0>                "}"                  { located $ special Tok.LBrace }
<0>                "["                  { located $ special Tok.RBrack }
<0>                "]"                  { located $ special Tok.LBrack }
<0>                "("                  { located $ special Tok.RParen }
<0>                ")"                  { located $ special Tok.LParen }
<0>                ";"                  { located $ special Tok.Semi }
<0>                ":"                  { located $ special Tok.Colon }
<0>                ","                  { located $ special Tok.Comma }

<0>                @ident               { located $ withLexeme Tok.Id }

<0>                @decimal             { located $ withLexeme (Tok.Int . read) }

<0>                \"                   { enterString `andBegin` state_string }
<state_string>     \"                   { leaveString `andBegin` 0 }
<state_string>     \\n                  { accumChar '\n' }
<state_string>     \\t                  { accumChar '\t' }
<state_string>     \\\\                 { accumChar '\\' }
<state_string>     \"                   { accumChar '\"' }
<state_string>     \\ $digit{3}         { accumAscii }
<state_string>     \\                   { \_ _ -> alexError "Illegal escape sequence" }
<state_string>     .                    { accumCurrent }
<state_string>     \n                   { \_ _ -> alexError "Illegal newline in string" }
<state_string>     \\ $whitechar+ \\    { skip }

<0>                \n                   { skip }
<0>                .                    { \_ _ -> alexError "Illegal charactor" }

{

{- Actions -}

located m i@(AlexPn offset _ _, _, _, _) len = do
  a <- m i len
  let !sp = Loc.Span offset (offset + len)
  pure $ Loc.At sp a

withLexeme f (_, _, _, str) len = pure $ f (take len str)

special = withLexeme . const

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
  setInString True
  alexMonadScan

leaveString (AlexPn offset _ _, _, _, _) _ = do
  s <- getStringAcc
  setInString False
  let !sp = Loc.Span (offset - length s - 1) (offset + 1)
  pure $ Loc.At sp (Tok.Str $ reverse s)

accumChar c _ _ = do
  modifyStringAcc (c :)
  alexMonadScan

accumCurrent i@(_, _, _, input) len =
  accumChar (head input) i len

accumAscii i@(_, _, _, input) len = do
  let !c = chr . read . take 3 . drop 1 $ input
  accumChar c i len

{- User state -}

data AlexUserState = AlexUserState
  { commentDepth :: Int,
    inString :: Bool,
    stringBuf :: String
  }

getCommentDepth = Alex \s@AlexState { alex_ust = ust } ->
  Right (s, commentDepth ust)

setCommentDepth = modifyCommentDepth . const

modifyCommentDepth f = Alex \s@AlexState { alex_ust = ust } ->
  Right (s { alex_ust = ust { commentDepth = f (commentDepth ust) } }, ())

getInString = Alex \s@AlexState { alex_ust = ust } ->
  Right (s, inString ust)

setInString b = Alex \s@AlexState { alex_ust = ust } ->
  Right (s { alex_ust = ust { inString = b } }, ())

getStringAcc = Alex \s@AlexState { alex_ust = ust } ->
  Right (s, stringBuf ust)

setStringAcc = modifyStringAcc . const

modifyStringAcc f = Alex \s@AlexState { alex_ust = ust } ->
  Right (s { alex_ust = ust { stringBuf = f (stringBuf ust) } }, ())

{- Definitions needed by alex -}

alexInitUserState = AlexUserState
  { commentDepth = 0,
    inString = False,
    stringBuf = ""
  }

alexEOF = do
  cd <- getCommentDepth
  when (cd > 0) do
    alexError "Unclosed comment at EOF"

  inStr <- getInString
  when inStr do
    alexError "Unclosed strign at EOF"

  (AlexPn offset _ _, _, _, _) <- alexGetInput
  pure $ Loc.At (Loc.Span offset offset) Tok.EOF

{- Execution -}

lex s = runAlex s (loop [])
  where
    loop toks = do
      tok@(Loc.At _ kind) <- alexMonadScan
      let toks' = tok : toks
      if kind == Tok.EOF
        then pure $ reverse toks'
        else loop toks'

}
