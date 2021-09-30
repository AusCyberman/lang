module Typecheck where

import AST


data TypeError = TypeError {
    expectedType :: LangType ,
    actualType :: LangType
    }



