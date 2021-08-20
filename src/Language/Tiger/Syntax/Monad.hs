{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Tiger.Syntax.Monad
  ( AlexInput (..),
    alexGetByte,
    alexPrevChar,
    ParserState (..),
    ParserM,
    Parser,
    runParser,
    getParserState,
    getInput,
    setInput,
    getLexState,
    setLexState,
    getCommentDepth,
    setCommentDepth,
    getInString,
    setInString,
    getStringBegin,
    setStringBegin,
    getStringBuf,
    setStringBuf,
    appendStringBuf,
    throw,
  )
where

import Codec.Binary.UTF8.String qualified as Utf8
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Data.Word
import Language.Tiger.Syntax.Errors qualified as Err

data AlexInput = AlexInput
  { offset :: {-# UNPACK #-} !Int,
    prev :: {-# UNPACK #-} !Char,
    bytes :: [Word8],
    rest :: T.Text
  }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput {..}
  | b : bs <- bytes = Just (b, input {bytes = bs})
  | Just (c, cs) <- T.uncons rest =
    let b : bs = Utf8.encode [c]
        !offset' = offset + 1
     in Just (b, AlexInput offset' c bs cs)
  | otherwise = Nothing

alexPrevChar :: AlexInput -> Char
alexPrevChar = prev

data ParserState = ParserState
  { input :: AlexInput,
    lexState :: {-# UNPACK #-} !Int,
    commentDepth :: {-# UNPACK #-} !Int,
    inString :: !Bool,
    stringBegin :: {-# UNPACK #-} !Int,
    stringBuf :: TB.Builder
  }

newtype ParserM e s a
  = Parser (forall r. s -> (s -> e -> r) -> (s -> a -> r) -> r)

type Parser = ParserM Err.Error ParserState

instance Functor (ParserM e s) where
  {-# INLINE fmap #-}
  fmap f (Parser k) =
    Parser $ \s kerr kok -> k s kerr (\s' a -> kok s' (f a))

instance Applicative (ParserM e s) where
  {-# INLINE pure #-}
  pure a = Parser $ \s _ kok -> kok s a

  {-# INLINE (<*>) #-}
  Parser kf <*> Parser k =
    Parser $ \s kerr kok ->
      kf s kerr $ \s' f ->
        k s' kerr $ \s'' a ->
          kok s'' (f a)

instance Monad (ParserM e s) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  Parser k >>= f =
    Parser $ \s kerr kok ->
      k s kerr $ \s' a ->
        let Parser k' = f a
         in k' s' kerr kok

runParser :: Parser a -> T.Text -> Either Err.Error a
runParser (Parser k) txt = k initialState (\_ e -> Left e) (\_ a -> Right a)
  where
    initialState =
      ParserState
        { input =
            AlexInput
              { offset = 0,
                prev = '\n',
                bytes = [],
                rest = txt
              },
          lexState = 0,
          commentDepth = 0,
          inString = False,
          stringBegin = 0,
          stringBuf = mempty
        }

getParserState :: Parser ParserState
getParserState = Parser $ \s _ kok -> kok s s

getInput :: Parser AlexInput
getInput = Parser $ \s@ParserState {input} _ kok -> kok s input

setInput :: AlexInput -> Parser ()
setInput input = Parser $ \s _ kok -> kok (s {input}) ()

getLexState :: Parser Int
getLexState = Parser $ \s@ParserState {lexState} _ kok -> kok s lexState

setLexState :: Int -> Parser ()
setLexState lexState = Parser $ \s _ kok -> kok (s {lexState}) ()

getCommentDepth :: Parser Int
getCommentDepth = Parser $ \s@ParserState {commentDepth} _ kok -> kok s commentDepth

setCommentDepth :: Int -> Parser ()
setCommentDepth commentDepth = Parser $ \s _ kok -> kok (s {commentDepth}) ()

getInString :: Parser Bool
getInString = Parser $ \s@ParserState {inString} _ kok -> kok s inString

setInString :: Bool -> Parser ()
setInString inString = Parser $ \s _ kok -> kok (s {inString}) ()

getStringBegin :: Parser Int
getStringBegin = Parser $ \s@ParserState {stringBegin} _ kok -> kok s stringBegin

setStringBegin :: Int -> Parser ()
setStringBegin stringBegin = Parser $ \s _ kok -> kok (s {stringBegin}) ()

getStringBuf :: Parser TB.Builder
getStringBuf = Parser $ \s@ParserState {stringBuf} _ kok -> kok s stringBuf

setStringBuf :: TB.Builder -> Parser ()
setStringBuf stringBuf = Parser $ \s _ kok -> kok (s {stringBuf}) ()

appendStringBuf :: TB.Builder -> Parser ()
appendStringBuf buf =
  Parser $ \s@ParserState {stringBuf} _ kok ->
    kok (s {stringBuf = stringBuf <> buf}) ()

throw :: e -> ParserM e s a
throw e = Parser $ \s kerr _ -> kerr s e
