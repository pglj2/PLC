import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--
evalForInit :: StateT -> ForInit -> StateTransformer Value
evalForInit env (NoInit) = return Nil
evalForInit env (VarInit []) = return Nil
evalForInit env (VarInit (x:xs)) = do
    varDecl env x >> evalForInit env (VarInit xs) -- se varDecl ou evalForInit for Nothing, será Nothing
evalForInit env (ExprInit expr) = do
    evalExpr env expr

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env NullLit = return Nil
evalExpr env (StringLit str) = return (String str)
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (ListExpr []) = return Nil
evalExpr env (ListExpr (x:xs)) = do
	evalExpr env x
	evalExpr env (ListExpr xs)
-- Evaluate lists
evalExpr env (ArrayLit list) = do
    case list of
        [] -> return (List [])
        (x:xs) -> do
            hd <- evalExpr env x
            (List tl) <- evalExpr env (ArrayLit xs)
            return (List ([hd]++tl))
-- Evaluate Negative Numbers
evalExpr env (PrefixExpr PrefixMinus expr) = do
	aux <- evalExpr env expr
	case aux of
		(Int int) -> return $ Int (-int)
		_ -> return $ Error "prefix minus invalid"
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    r <- stateLookup env var -- crashes if the variable doesn't exist -> crasha mais nao :D
    case r of
        -- variable not defined
        (Error _) -> do
            varDecl env (VarDecl (Id var) (Nothing))
            auxv <- evalExpr env expr
            setVar var auxv
        -- variable defined
        _   -> do
            e <- evalExpr env expr
            setVar var e

--Evaluate increment/decrement value
evalExpr env (UnaryAssignExpr inc (LVar var)) = do 
    case inc of
        (PrefixInc) -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1))) 
        (PrefixDec) -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) (IntLit 1)))
        (PostfixInc) -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1)))
        (PostfixDec) -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) (IntLit 1)))
        
--Evaluate Functions


--Evaluate Statements        
evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr

--Evaluate ifsingle statement
evalStmt env (IfSingleStmt exp stmt) = do
	bol <- evalExpr env exp
	case bol of
		(Bool b) -> if b == True then evalStmt env stmt else return Nil --retorno a ser modificado!
		_ -> return $ Error "not a boolean expression"

--Evaluate while statement		
evalStmt env (WhileStmt exp stmt) = do
	aval <- evalExpr env exp
	case aval of
		  (Bool True) -> do
	              	stt <- (evalStmt env stmt)
              		case (stt) of 
              		  (Break) -> return Nil 
              		  _ -> do 
            		          ret <-(evalStmt env (WhileStmt exp stmt)) 
            		          return ret
		  (Bool False) -> return Nil			
		  _ -> return $ Error "not a boolean expression" 
		  
--Evaluate DoWhile statement
evalStmt env (DoWhileStmt stmt exp) = do
  aval <- evalExpr env exp
  stt <- evalStmt env stmt
  case aval of
		  (Bool True) -> do
              		case (stt) of 
              		  (Break) -> return Nil 
              		  _ -> do 
            		          ret <-(evalStmt env (DoWhileStmt stmt exp)) 
            		          return ret
		  (Bool False) -> return Nil
		  _ -> return $ Error "not a boolean expression" 
		  

-- Evaluate if else statement
evalStmt env (IfStmt expr ifBlock elseBlock) = do
    condition <- evalExpr env expr
    case condition of
        (Bool cond) -> if (cond) then do
        ret <- (evalStmt env ifBlock)
        return ret
         else do 
            ret <- (evalStmt env elseBlock)
            return ret
        (Error _) -> return $ Error ("Condition error")

-- Evaluate BlockStmt
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (x:xs)) = do
  ret <- evalStmt env x
  case ret of
        (Break) -> return Break
        _ -> do
         return ret
         evalStmt env (BlockStmt xs)

-- Evaluate ForStmt
-- exptest = test/condition ; expinc = increment ; stmt = statement/body; 
evalStmt env (ForStmt initialize exptest expinc stmt) = do 
     evalForInit env initialize 
     case exptest of
          Nothing -> return Nil
          (Just  exp) -> do
            check <- evalExpr env exp
            if (check == (Bool True)) then do 
            evalStmt env stmt
            case expinc of
                 Nothing -> return Nil
                 (Just exp) -> do
                 evalExpr env exp
                 evalStmt env (ForStmt NoInit exptest expinc stmt)
            else return Nil

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

--
-- Environment and auxiliary functions
--

environment :: Map String Value 
environment = Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> error $ "Variable " ++ show var ++ " not defiend."
        Just val -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value --mudar para lista
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union defs environment) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
