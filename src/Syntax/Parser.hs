{-# LANGUAGE  OverloadedStrings #-}
module Syntax.Parser where

import Syntax.Lexer
import Text.Megaparsec
import Text.Megaparsec.Char

import Syntax.AST

data TypeCheckError =  
    TypeCheckError 
    { expectedType :: Type 
    , actualType :: Type
    }

type Parser = ParsecT TypeCheckError Text (StateT Context)

expr :: Parser Expr
expr = try structExpr <|> try assignExpr <|> try block <|> try functionExpr <|> try callExpr <|> parens expr

assignExpr :: Parser Expr
assignExpr = Assign <$> identifier <* equals <*> expr


typeSignature :: Parser (Identifier, LangType)
typeSignature = (,) <$> identifier <* space1 <* char ':' <* space1 <*> langType

langType :: Parser LangType
langType =
    try
        ( Type <$> identifier
            <*> some (space *> try (SingularType <$> identifier) <|> parens langType)
        )
        <|> (SingularType <$> identifier)

identifier :: Parser Identifier
identifier = Identifier <$> ident

structExpr :: Parser Expr
structExpr = StructExpr . Struct <$> braces (commaSep1 $ (,) <$> identifier <* spaces <* char '=' <* spaces <*> expr)

varExpr :: Parser Expr
varExpr = Var <$> identifier

lineBlock :: Parser Expr
lineBlock = try assignExpr <|> try callExpr

callExpr :: Parser Expr
callExpr = Call <$> identifier <*> parens (commaSep expr)

block :: Parser Expr
block = Block <$> braces (try ((++) <$> many expr <*> ((: []) <$> expr)) <|> pure [])

functionExpr :: Parser Expr
functionExpr = string "fn" 
    *> space1 *> (Function <$> identifier <* space1 <*> parens (typeSignature) <* space <*> (try (Just <$ char ':' <*  spaces <*> langType) <|> pure Nothing) <*>  block)

assertType :: LangType -> LangType -> Parser ()
assertType = undefined 

checkExpr :: Expr -> Parser (Expr,Type)
checkExpr (Function name types returnType blocks = do
    ctx <- gets
    puts (ctx++types)
    mapM checkExpr blocks
    lastType <- gets last
    
    