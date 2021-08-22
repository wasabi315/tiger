{

module Language.Tiger.Syntax.Lexer
  ( lex,
    lexer
  )
where

import Prelude hiding (lex)
import Control.Monad
import Data.Char
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Read qualified as T
import Language.Tiger.Bug qualified as Bug
import Language.Tiger.Syntax.Errors qualified as Err
import Language.Tiger.Syntax.Location qualified as Loc
import Language.Tiger.Syntax.Monad
import Language.Tiger.Syntax.Token qualified as Tok

}

$whitechar = [\ \t\n\r\f\v]

$digit = 0-9
$alpha = [a-zA-Z]

@ident = $alpha [$alpha $digit \_]*

@decimal = $digit+

tokens :-

<0>                $white+              ;
<0>                "/*"                 { enterComment `andBegin` stateComment }
<stateComment>     "/*"                 { enterComment }
<stateComment>     "*/"                 { leaveComment }
<0>                "*/"                 { errorToken' Err.IllegalCommentClose }
<stateComment>     .                    ;
<stateComment>     \n                   ;

<0>                "type"               { special Tok.Type }
<0>                "var"                { special Tok.Var }
<0>                "function"           { special Tok.Func }
<0>                "of"                 { special Tok.Of }
<0>                "end"                { special Tok.End }
<0>                "in"                 { special Tok.In }
<0>                "nil"                { special Tok.Nil }
<0>                "let"                { special Tok.Let }
<0>                "do"                 { special Tok.Do }
<0>                "to"                 { special Tok.To }
<0>                "for"                { special Tok.For }
<0>                "while"              { special Tok.While }
<0>                "break"              { special Tok.Break }
<0>                "else"               { special Tok.Else }
<0>                "then"               { special Tok.Then }
<0>                "if"                 { special Tok.If }
<0>                "array"              { special Tok.Array }

<0>                ":="                 { special Tok.Assign }
<0>                "|"                  { special Tok.Or }
<0>                "&"                  { special Tok.And }
<0>                ">="                 { special Tok.Ge }
<0>                ">"                  { special Tok.Gt }
<0>                "<="                 { special Tok.Le }
<0>                "<"                  { special Tok.Lt }
<0>                "<>"                 { special Tok.Neq }
<0>                "="                  { special Tok.Eq }
<0>                "/"                  { special Tok.Div }
<0>                "*"                  { special Tok.Mul }
<0>                "-"                  { special Tok.Sub }
<0>                "+"                  { special Tok.Add }
<0>                "."                  { special Tok.Dot }
<0>                "{"                  { special Tok.LBrace }
<0>                "}"                  { special Tok.RBrace }
<0>                "["                  { special Tok.LBrack }
<0>                "]"                  { special Tok.RBrack }
<0>                "("                  { special Tok.LParen }
<0>                ")"                  { special Tok.RParen }
<0>                ";"                  { special Tok.Semi }
<0>                ":"                  { special Tok.Colon }
<0>                ","                  { special Tok.Comma }

<0>                @ident               { token Tok.Id }

<0>                @decimal             { token (Tok.Int . read . T.unpack) }

<0>                \"                   { enterString `andBegin` stateString }
<stateString>      \"                   { leaveString `andBegin` 0 }
<stateString>      \\n                  { accumChar '\n' }
<stateString>      \\t                  { accumChar '\t' }
<stateString>      \\\\                 { accumChar '\\' }
<stateString>      \"                   { accumChar '\"' }
<stateString>      \\ $digit{3}         { accumAscii }
<stateString>      \\.                  { errorToken Err.IllegalEscapeSequence }
<stateString>      .                    { accumCurrent }
<stateString>      \n                   { errorToken' Err.NewlineInString }
<stateString>      \\ $whitechar+ \\    ;

<0>                \n                   ;
<0>                .                    { errorToken Err.UnknownToken }

{

type Action = AlexInput -> Int -> Parser Tok.Token

{- Utilities -}

andBegin :: Action -> Int -> Action
andBegin action lexState input len = do
  setLexState lexState
  action input len

token :: (T.Text -> Tok.Token_) -> Action
token f AlexInput {..} len = do
  let sp = Loc.Span offset (offset + len)
      str = T.take (fromIntegral len) rest
  pure $ Loc.At sp (f str)

special :: Tok.Token_ -> Action
special = token . const

errorToken :: (T.Text -> Err.ErrorKind) -> Action
errorToken f AlexInput {..} len = do
  let sp = Loc.Span offset (offset + len)
      str = T.take (fromIntegral len) rest
  throw $ Loc.At sp (f str)

errorToken' :: Err.ErrorKind -> Action
errorToken' = errorToken . const

continue = munch

{- Actions -}

enterComment :: Action
enterComment _ _ = do
  cd <- getCommentDepth
  setCommentDepth (cd + 1)
  continue

leaveComment :: Action
leaveComment _ _ = do
  cd <- getCommentDepth
  setCommentDepth (cd - 1)
  when (cd == 1) $ do
    setLexState 0
  continue

enterString :: Action
enterString AlexInput {offset} _ = do
  setStringBuf mempty
  setStringBegin offset
  setInString True
  continue

leaveString :: Action
leaveString AlexInput {offset} _ = do
  buf <- getStringBuf
  begin <- getStringBegin
  setInString False
  let sp = Loc.Span begin (offset + 1)
      str = TB.toLazyText buf
  pure $ Loc.At sp (Tok.Str str)

accumChar :: Char -> Action
accumChar c _ _ = do
  appendStringBuf $ TB.singleton c
  continue

accumCurrent :: Action
accumCurrent input@AlexInput {rest} len = do
  when (len /= 1) $
    Bug.compilerBug "Invalid accumCurrent call"
  accumChar (T.head rest) input len

accumAscii :: Action
accumAscii input@AlexInput {..} len = do
  when (len /= 4) $
    Bug.compilerBug "Invalid accumAscii call"
  let subStr = T.take 3 . T.drop 1 $ rest
  let !dec = case T.decimal subStr of
        Right (d, "") -> d
        _ -> Bug.compilerBug "Invalid accumAscii call"
  when (dec >= 256) $ do
    let sp = Loc.Span offset (offset + len)
    throw $ Loc.At sp (Err.IllegalAsciiValue dec)
  accumChar (chr dec) input len

{- Execution -}

munch :: Parser Tok.Token
munch = do
  ParserState {..} <- getParserState

  case alexScan input lexState of
    AlexEOF -> do
      let sp = Loc.Span (offset input) (offset input)
      when (commentDepth > 0) $
        throw $ Loc.At sp Err.UnclosedComment
      when inString $
        throw $ Loc.At sp Err.UnclosedString
      pure $ Loc.At sp Tok.EOF
    
    AlexError input' -> do
      let sp = Loc.Span (offset input') (offset input' + 1)
      throw $ Loc.At sp Err.UnknownLexicalError

    AlexSkip input' _ -> do
      setInput input'
      munch

    AlexToken input' len action -> do
      setInput input'
      action (input {bytes = []}) len

lex :: T.Text -> Either Err.Error [Tok.Token]
lex = runParser (loop [])
  where
    loop toks = do
      tok@(Loc.At _ kind) <- munch
      let toks' = tok : toks
      if kind == Tok.EOF
        then pure $ reverse toks'
        else loop toks'

lexer :: (Tok.Token -> Parser a) -> Parser a
lexer k = munch >>= k

}
