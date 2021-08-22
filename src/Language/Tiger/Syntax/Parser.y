{

{-# LANGUAGE ViewPatterns #-}

module Language.Tiger.Syntax.Parser
  ( parse,
  )
where

import Control.Comonad
import Data.Text.Lazy qualified as T
import Language.Tiger.Syntax.Errors qualified as Err
import Language.Tiger.Syntax.Location qualified as Loc
import Language.Tiger.Syntax.Token qualified as Tok
import Language.Tiger.Syntax.AST qualified as A
import Language.Tiger.Syntax.Lexer qualified as Lex
import Language.Tiger.Syntax.Monad as P

}

%name tiger

%tokentype { Tok.Token }
%monad { P.Parser }
%lexer { Lex.lexer } { Loc.At _ Tok.EOF }
%error { parseError }
%errorhandlertype explist

%token
  type        { Loc.At _ Tok.Type }
  var         { Loc.At _ Tok.Var }
  function    { Loc.At _ Tok.Func }
  of          { Loc.At _ Tok.Of }
  end         { Loc.At _ Tok.End }
  in          { Loc.At _ Tok.In }
  nil         { Loc.At _ Tok.Nil }
  let         { Loc.At _ Tok.Let }
  do          { Loc.At _ Tok.Do }
  to          { Loc.At _ Tok.To }
  for         { Loc.At _ Tok.For }
  while       { Loc.At _ Tok.While }
  break       { Loc.At _ Tok.Break }
  else        { Loc.At _ Tok.Else }
  then        { Loc.At _ Tok.Then }
  if          { Loc.At _ Tok.If }
  array       { Loc.At _ Tok.Array }
  ':='        { Loc.At _ Tok.Assign }
  '|'         { Loc.At _ Tok.Or }
  '&'         { Loc.At _ Tok.And }
  '>='        { Loc.At _ Tok.Ge }
  '>'         { Loc.At _ Tok.Gt }
  '<='        { Loc.At _ Tok.Le }
  '<'         { Loc.At _ Tok.Lt }
  '<>'        { Loc.At _ Tok.Neq }
  '='         { Loc.At _ Tok.Eq }
  '/'         { Loc.At _ Tok.Div }
  '*'         { Loc.At _ Tok.Mul }
  '-'         { Loc.At _ Tok.Sub }
  '+'         { Loc.At _ Tok.Add }
  '.'         { Loc.At _ Tok.Dot }
  '{'         { Loc.At _ Tok.LBrace }
  '}'         { Loc.At _ Tok.RBrace }
  '['         { Loc.At _ Tok.LBrack }
  ']'         { Loc.At _ Tok.RBrack }
  '('         { Loc.At _ Tok.LParen }
  ')'         { Loc.At _ Tok.RParen }
  ';'         { Loc.At _ Tok.Semi }
  ':'         { Loc.At _ Tok.Colon }
  ','         { Loc.At _ Tok.Comma }
  str         { (atStr -> Just $$) }
  int         { (atInt -> Just $$) }
  ident       { (atId -> Just $$) }

%nonassoc ':='
%nonassoc ident
%nonassoc while do
%right then
%right else
%nonassoc '[' ']' of
%left '|'
%left '&'
%nonassoc '=' '<>' '>=' '>' '<=' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

Program
  : Expr                                                             { $1 }

Expr :: { A.Expr }
  : Lvalue                                                           { extend A.Var $1 }
  | nil                                                              { A.Nil <\$ $1 }
  | int                                                              { fmap A.Int $1 }
  | str                                                              { fmap A.Str $1 }
  | ident '(' SepBy(',', Expr) ')'                                   { Loc.merge $1 $4 (A.Call (extract $1) $3) }
  | Expr '|' Expr                                                    { Loc.merge $1 $3 (A.If $1 (A.Int 1 <\$ $2) (Just $3)) }
  | Expr '&' Expr                                                    { Loc.merge $1 $3 (A.If $1 $3 (Just $ A.Int 0 <\$ $2)) }
  | Expr '=' Expr                                                    { Loc.merge $1 $3 (A.Op A.Eq $1 $3) }
  | Expr '<>' Expr                                                   { Loc.merge $1 $3 (A.Op A.Neq $1 $3) }
  | Expr '>=' Expr                                                   { Loc.merge $1 $3 (A.Op A.Ge $1 $3) }
  | Expr '>' Expr                                                    { Loc.merge $1 $3 (A.Op A.Gt $1 $3) }
  | Expr '<=' Expr                                                   { Loc.merge $1 $3 (A.Op A.Le $1 $3) }
  | Expr '<' Expr                                                    { Loc.merge $1 $3 (A.Op A.Lt $1 $3) }
  | Expr '+' Expr                                                    { Loc.merge $1 $3 (A.Op A.Add $1 $3) }
  | Expr '-' Expr                                                    { Loc.merge $1 $3 (A.Op A.Sub $1 $3) }
  | Expr '*' Expr                                                    { Loc.merge $1 $3 (A.Op A.Mul $1 $3) }
  | Expr '/' Expr                                                    { Loc.merge $1 $3 (A.Op A.Div $1 $3) }
  | '-' Expr %prec NEG                                               { Loc.merge $1 $2 (A.Op A.Sub (A.Int 0 <\$ $1) $2) }
  | ident '{' SepBy(',', RecordField) '}'                            { Loc.merge $1 $4 (A.Record $3 (extract $1)) }
  | ident '[' Expr ']' of Expr                                       { Loc.merge $1 $6 (A.Array (extract $1) $3 $6) }
  | Lvalue ':=' Expr                                                 { Loc.merge $1 $3 (A.Assign $1 $3) }
  | '(' SepBy(';', Expr) ')'                                         { Loc.merge $1 $3 (A.Seq $2) }
  | let Decls in SepBy(';', Expr) end                                { Loc.merge $1 $5 (A.Let $2 (Loc.merge $3 $5 (A.Seq $4))) }
  | if Expr then Expr else Expr                                      { Loc.merge $1 $6 (A.If $2 $4 (Just $6)) }
  | if Expr then Expr                                                { Loc.merge $1 $4 (A.If $2 $4 Nothing) }
  | while Expr do Expr                                               { Loc.merge $1 $4 (A.While $2 $4) }
  | for ident ':=' Expr to Expr do Expr                              { Loc.merge $1 $8 (A.For (fmap A.VName $2) $4 $6 $8) }
  | break                                                            { A.Break <\$ $1 }
  | '(' Expr ')'                                                     { $2 }

Decls :: { [A.Decl] }
  : List1(FnDecl) NonFnDecl                                          { A.DFn $1 : $2 }
  | VarDecl Decls                                                    { A.DVar $1 : $2 }
  | List1(TyDecl) NonTyDecl                                          { A.DTy $1 : $2 }
  | {- empty -}                                                      { [] }

NonFnDecl :: { [A.Decl] }
  : VarDecl Decls                                                    { A.DVar $1 : $2 }
  | List1(TyDecl) NonTyDecl                                          { A.DTy $1 : $2 }
  | {- empty -}                                                      { [] }

NonTyDecl :: { [A.Decl] }
  : VarDecl Decls                                                    { A.DVar $1 : $2 }
  | List1(FnDecl) NonFnDecl                                          { A.DFn $1 : $2 }
  | {- empty -}                                                      { [] }

FnDecl :: { A.FnDecl }
  : function ident '(' SepBy(',', TyField) ')' '=' Expr              { Loc.merge $1 $7 (A.FnDecl (extract $2) $4 Nothing $7) }
  | function ident '(' SepBy(',', TyField) ')' ':' ident '=' Expr    { Loc.merge $1 $7 (A.FnDecl (extract $2) $4 (Just $7) $9) }

VarDecl :: { A.VarDecl }
  : var ident ':=' Expr                                              { Loc.merge $1 $4 (A.VarDecl (extract $2) Nothing $4) }
  | var ident ':' ident ':=' Expr                                    { Loc.merge $1 $6 (A.VarDecl (extract $2) (Just $4) $6) }

TyDecl :: { A.TyDecl }
  : type ident '=' Ty                                                { Loc.merge $1 $4 (A.TyDecl (extract $2) $4) }

Ty :: { A.Type }
  : ident                                                            { fmap A.TName $1 }
  | '{' SepBy(',', TyField) '}'                                      { Loc.merge $1 $3 (A.TRecord $2)}
  | array of ident                                                   { Loc.merge $1 $3 (A.TArray $ extract $3) }

Lvalue :: { A.Var }
  : ident                                                            { fmap A.VName $1 }
  | Lvalue '.' ident                                                 { Loc.merge $1 $3 (A.VField $1 (extract $3)) }
  | ident '[' Expr ']'                                               { Loc.merge $1 $4 (A.VIxed (fmap A.VName $1) $3) }
  | Lvalue '[' Expr ']'                                              { Loc.merge $1 $4 (A.VIxed $1 $3) }

TyField :: { Loc.Located (T.Text, T.Text) }
  : ident ':' ident                                                  { Loc.merge $1 $3 (extract $1, extract $3) }

RecordField :: { Loc.Located (T.Text, A.Expr) }
  : ident '=' Expr                                                   { Loc.merge $1 $3 (extract $1, $3) }

List(X)
  : {- empty -}                                                      { [] }
  | List1(X)                                                         { $1 }

List1(X)
  : X List(X)                                                        { $1 : $2 }

SepBy(SEP, X)
  : {- empty -}                                                      { [] }
  | SepBy1(SEP, X)                                                   { $1 }

SepBy1(SEP, X)
  : X %shift                                                         { [$1] }
  | X SEP SepBy1(SEP, X)                                             { $1 : $3 }

{

atStr (Loc.At sp (Tok.Str s)) = Just (Loc.At sp s)
atStr _ = Nothing

atInt (Loc.At sp (Tok.Int n)) = Just (Loc.At sp n)
atInt _ = Nothing

atId (Loc.At sp (Tok.Id s)) = Just (Loc.At sp s)
atId _ = Nothing

parseError :: (Tok.Token, [String]) -> Parser a
parseError (tok, explist) =
  throw $ Err.ExpectOneOfBut explist <$> tok

parse :: T.Text -> Either Err.Error A.Expr
parse = runParser tiger

}
