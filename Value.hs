module Value (Value (..)) where

import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | GlobalVar  
    | Var String
    | Nil
    | List [Value]
    | Func Id [Id] [Statement] --para avaliar funções
    | Error String
    | Return Value 
    | Break (Maybe Id)--talvez seja necessário para o BreakStmt, no dia que eu entender eu aviso :v	
    deriving(Eq)

--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = show name
  show Nil = "undefined"
  show (Error st) = show st
  show (Return v) = show v
  show (Func (Id id) ids stmts) = show id ++ showArguments ids    
  show (List list) = showListContents list -- a função de baixo estava sendo inutilizada, talvez seja mais interessante deixar desta forma

showArguments:: [Id] -> String  
showArguments [] = ""
showArguments [(Id a)] = show a 
showArguments ((Id a):as) = show a ++ ", " ++ showArguments as
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
