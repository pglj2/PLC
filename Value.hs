module Value (Value (..)) where

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Nil
    | List [Value]
    | Error String
    | Break --talvez seja necessário para o BreakStmt, no dia que eu entender eu aviso :v	

--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show Nil = "undefined"
  show (Error st) = show st
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
