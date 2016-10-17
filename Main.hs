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
        
evalExpr env (DotRef exp (Id id)) = do 
    aval <- evalExpr env exp
    case aval of
        List l -> do 
            case id of
                "head" -> nossoHead l 
                "tail" -> nossoTail l
                _ -> return $ Error "Cannot use this function"

--Evaluate Functions
evalExpr env (CallExpr exp listexp) = 
            case exp of
                DotRef expres (Id id) -> do
                    lista <- evalExpr env expres
                    case lista of
                        List l -> do
                            case id of
                                "concat" -> nossoConcat l listexp
                                "head" -> nossoHead l
                                "tail" -> nossoTail l
                                _ -> return $ Error "Function not available!" --falta fazer o len!
                        _ -> do
                            aval <- evalExpr env exp 
                            case aval of
                                Func id ids stmts -> do 
                                    poeEscopo
                                    retorno <- evalStmt env (BlockStmt stmts)
                                    tiraEscopo
                                    case retorno of
                                        (Break) -> return $ Error "Cannot insert break here"
                                        _       -> return Nil  


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
		(Bool b) -> if (b) then evalStmt env stmt else return Nil
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

evalStmt env (FunctionStmt (Id id) ids stmts) = variaveisLocais id (Func (Id id) ids stmts)

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--functions for lists ; falta testar!
nossoHead:: Value -> StateTransformer Value
nossoHead (List []) = return Nil
nossoHead (List (a:as)) = return a

nossoTail:: Value -> StateTransformer Value
nossoTail (List []) = return Nil
nossoTail (List (a:as)) = return (List as) 

nossoConcat:: Value -> Value -> StateTransformer Value
nossoConcat (List b) (List []) = return (List b)
nossoConcat (List as) (List (b:bs)) = 
                                        case b of
                                            (List x) -> nossoConcat (List (as++x)) (List bs) --concatenar lista de listas
                                            c -> nossoConcat (List (as++[c])) (List bs)  

variaveisLocais:: String -> Value -> StateTransformer Value  
variaveisLocais nome val = ST $ \s -> (val, (insert nome val (head s)):(tail s)) 

variaveisGlobais:: String -> Value -> StateT -> StateT
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

environment :: StateT
environment = [Map.empty]

lookupEscopo:: StateT -> String -> Maybe Value
lookupEscopo [] _ = Nothing
lookupEscopo (a:as) variavel = case Map.lookup variavel a of
                                    (Nothing) -> lookupEscopo as variavel
                                    (Just b) -> Just b   

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    case lookupEscopo s var of
        Nothing -> error $ "Variable " ++ show var ++ " not defined."
        Just val -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, auxInsert var val s)

auxInsert:: String -> Value -> StateT -> StateT
auxInsert _ _ [] = error $ "Variable doesn't exist"
auxInsert variavel value env = case (Map.lookup variavel (head env)) of
                                    Nothing -> (head env):(auxInsert variavel value (tail env))
                                    (Just a) -> (insert variavel value (head env)):(tail env)
--
-- Types and boilerplate
--

type StateT = [Map String Value] --mudando para lista, pois assim será possível trabalhar com escopos
data StateTransformer t = ST (StateT -> (t, StateT))

poeEscopo:: StateTransformer Value
poeEscopo = ST (\x -> (Nil, (Map.empty):x)) 

tiraEscopo:: StateTransformer Value
tiraEscopo = ST $ \x -> (Nil,(tail x))

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
showResult (val, []) = ""
showResult (val, (a:as)) = show val ++ "\n" ++ show (toList $ union a (Map.empty)) ++ "\n" 
                            ++ showResult (val,as)

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f [Map.empty]

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
