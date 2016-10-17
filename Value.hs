module Value (Value (..)) where

import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Nil
    | List [Value]
    | Func Id [Id] [Statement] --para avaliar funções
    | Error String
    | Break --talvez seja necessário para o BreakStmt, no dia que eu entender eu aviso :v	
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
  show (List list) = showListContents list -- a função de baixo estava sendo inutilizada, talvez seja mais interessante deixar desta forma
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
