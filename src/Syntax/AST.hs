{-# LANGUAGE GADTs #-}
module Syntax.AST where
import Data.Text (Text)
import qualified Data.Text as T
data PrimTypes = StringType | FloatType Int | UnsignedIntType Int | SignedIntType Int deriving (Eq, Show)









newtype Identifier =
  Identifier Text deriving (Show,Eq)


data LangType
  = StructType StructTemplate
  | SingularType Identifier
  | Type Identifier [LangType]
  deriving (Eq,Show)

type Context = [(Identifier, LangType)]







data Expr  = Double Double  | Int Int | String Text  | Op Identifier Expr Expr
  | StructExpr Struct
  | Declaration Identifier (Maybe LangType)
  | Assign Identifier Expr
  | Var Identifier
  | Block [Expr]
  | Function Identifier Context (Maybe LangType)  Expr
  | Call Identifier [Expr]
  deriving (Eq,Show)



data EnumType = EnumOne Identifier | TupleEnum Identifier [LangType] | StructEnum Identifier [LangType]



newtype StructTemplate = StructTemplate [(Identifier, LangType)] deriving (Eq,Show)



newtype Struct = Struct [(Identifier, Expr)] deriving (Eq,Show)



data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq,Show)

