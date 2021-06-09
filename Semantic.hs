module Semantic where

import Data.Map as Map
import Data.Array
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import System.IO
import System.Exit ( exitFailure, exitSuccess )

import EnvStateHandler
import AbsMyLang
import Utils

class InterpretableProg a where
  interpretate :: a -> IO ()

instance InterpretableProg Program where
  interpretate (Prog topDefs) = do
    res <- runExceptT $ runStateT (runReaderT (interprateAllTopDefs topDefs) initialEnv) initialState
    case res of
      Left error -> print error
      Right _ -> return ()


interprateAllTopDefs :: [TopDef] -> EnvState ()
interprateAllTopDefs [] = runMain
interprateAllTopDefs (topDef:topDefs) = do
  env <- interprateTopDef topDef
  local (const env) (interprateAllTopDefs topDefs)

interprateTopDef :: TopDef -> EnvState Env
interprateTopDef (FnDef varType ident args block) = do
  env <- ask
  state <- get
  loc <- newloc
  validateIdent ident
  if areArgsUnique args
    then
      if validArgsTypes args
        then do
          let newEnv = Map.insert ident loc env
          modify $ Map.insert loc (False, StorableFun (newEnv, varType, args, block))
          return newEnv
      else
        throwError $ "Argument of type void in function" ++ showIdent ident
    else
      throwError $ "Argument repetitions in function" ++ showIdent ident

runMain :: EnvState ()
runMain = do
  env <- ask
  (_ , StorableFun (_, retType, _, block)) <- getVal (Ident "main")
  functionsMeetUp
  newEnv <- interprateBlockStart block
  (_, mainRet) <- getVal (Ident "__returnValue__")
  if allTypesMatch retType mainRet
    then local (const newEnv) (return ())
    else case mainRet of
      Undefined -> throwError "Main did not returned: Add return or check use of break command"
      StorableVoid -> throwError "Main did not retruned value" 
      _ -> throwError "Main return value does not match"

interprateBlockStart :: Block -> EnvState Env
interprateBlockStart (Bloc b) = do
  validateBlock (Bloc b)
  interprateBlock (Bloc b)
  ask

interprateBlock :: Block -> EnvState Env
interprateBlock (Bloc []) = do
  modify $ Map.insert continueKey (True, StorableBool False)
  ask

interprateBlock (Bloc (s:ss)) = do
  env <- interprateStmt s
  (_, StorableBool breakOn) <- getVal (Ident "__breakState__")
  (_, StorableBool continueOn) <- getVal (Ident "__continueState__")
  isReturnOn <- isReturnOnF
  if isReturnOn || breakOn || continueOn
    then return env
    else local (const env) (interprateBlock (Bloc ss))

declare :: Bool -> Type -> [Item] -> EnvState Env
declare _ _ [] = ask
declare isReadOnly (SimpleType sType) (NoInit i : is) = do
  env <- ask
  state <- get
  loc <- newloc
  case sType of
    Int -> modify $ Map.insert loc (isReadOnly, UndeclaredInt)
    Str -> modify $ Map.insert loc (isReadOnly, UndeclaredString)
    Bool -> modify $ Map.insert loc (isReadOnly, UndeclaredBool)
    Void -> throwError $ "Can't declare void as type" ++ showIdent i
  local (const $ Map.insert i loc env) (declare isReadOnly (SimpleType sType) is)

declare isReadOnly (SimpleType sType) (Init i e : is) = do
  env <- ask
  state <- get
  loc <- newloc
  evalved <- evalExp e
  if doesTypesMatch sType evalved
    then modify $ Map.insert loc (isReadOnly, evalved)
    else throwError $ "Types do not match" ++ showIdent i
  local (const $ Map.insert i loc env) (declare isReadOnly (SimpleType sType) is)

declare isReadOnly (SimpleType _) (ArrayInit i [] : is) = throwError $ "Cannot intialize an empty array" ++ showIdent i
declare isReadOnly (SimpleType sType) (ArrayInit i e : is) = do
  env <- ask
  state <- get
  loc <- newloc
  evalved <- mapM evalExp e
  if checkList evalved
    then do
      declare isReadOnly (SimpleType sType) is
      modify $ Map.insert loc (False, Undefined)
      local (const $ Map.insert i loc env) (fillArray sType i evalved)
    else throwError $ "Array parameter are not integer type" ++ (showIdent i)

declare isReadOnly (CollectionType (Array t)) (Init i e : is) = do
  env <- ask
  state <- get
  loc <- newloc
  evalved <- evalExp e
  if arrTypesMatch t evalved
    then modify $ Map.insert loc (isReadOnly, evalved)
    else throwError $ "Types do not match" ++ showIdent i
  local (const $ Map.insert i loc env) (declare isReadOnly (CollectionType (Array t)) is)

fillArray :: SType -> Ident -> [Storable] -> EnvState Env
fillArray sType i [] = ask
fillArray sType i [StorableInt int] = do
  state <- get
  loc <- getLoc i
  (_, val) <- getVal i
  case val of
    Undefined -> modify $ Map.insert loc (False, StorableArr (replicate (fromIntegral int) (mapSTypeToStorable sType)))
    (StorableArr arr) -> modify $ Map.insert loc (False, newArr)
      where newArr = iterativeFill (fromIntegral int) arr (mapSTypeToStorable sType)
  ask

fillArray sType i (StorableInt int : es) = do
  state <- get
  loc <- getLoc i
  (_, val) <- getVal i
  case val of
    Undefined -> modify $ Map.insert loc (False, StorableArr (replicate (fromIntegral int) Undefined))
    (StorableArr arr) -> modify $ Map.insert loc (False, newArr)
      where newArr = iterativeFill (fromIntegral int) arr Undefined
  fillArray sType i es

iterativeFillFoo :: Int -> Storable -> Storable -> Storable
iterativeFillFoo int typeOf x = case x of
  Undefined -> StorableArr (replicate int typeOf)
  (StorableArr arr) -> iterativeFill int arr typeOf

iterativeFill :: Int -> [Storable] -> Storable -> Storable
iterativeFill int arr typeOf = StorableArr $ Prelude.map (iterativeFillFoo int typeOf) arr

interprateStmt :: Stmt -> EnvState Env
interprateStmt Empty = ask

interprateStmt (BStmt blok) = interprateBlockStart blok

interprateStmt (Decl varType items) = declare False varType items
interprateStmt (ConstDecl varType items) = declare True varType items

interprateStmt (Ass i e) = do
  env <- ask
  state <- get
  (isReadOnly, val) <- getVal i
  evalved <- evalExp e
  loc <- getLoc i
  if isReadOnly
    then throwError $ "Read only variable" ++ showIdent i
    else if not $ doesAssTypesMatch evalved val
       then throwError $ "Types do not match" ++ showIdent i
       else do
        modify $ Map.insert loc (isReadOnly, evalved)
        return env

interprateStmt Break = do
  modify $ Map.insert breakKey (True, StorableBool True)
  ask

interprateStmt Continue = do
  modify $ Map.insert continueKey (True, StorableBool True)
  ask

interprateStmt (Ret e) = do
  eval <- evalExp e
  modify $ Map.insert returnValueKey (True, eval)
  ask

interprateStmt VRet = do
  modify $ Map.insert returnValueKey (True, StorableVoid)
  ask

interprateStmt (Cond e st) = do
  evalved <- evalExp e
  validateLoopAssign st "If"
  case evalved of
    (StorableBool True) -> interprateStmt st
    (StorableBool False) -> ask
    _ -> throwError "If condition must be a boolean"

interprateStmt (CondElse e blT blF) = do
  evalved <- evalExp e
  case evalved of
    (StorableBool True) -> interprateBlockStart blT
    (StorableBool False) -> interprateBlockStart blF
    _ -> throwError "If condition must be a boolean"

interprateStmt (While e s) = do
  evalved <- evalExp e
  validateLoopAssign s "While"
  case evalved of
    (StorableBool True) -> do
      interprateStmt s
      (_, StorableBool breakOn) <- getVal (Ident "__breakState__")
      isReturnOn <- isReturnOnF
      if isReturnOn || breakOn
        then do
          modify $ Map.insert breakKey (True, StorableBool False)
          modify $ Map.insert continueKey (True, StorableBool False)
          ask
        else do
          modify $ Map.insert continueKey (True, StorableBool False)
          interprateStmt (While e s)
          ask
    (StorableBool False) -> ask
    _ -> throwError "If condition must be a boolean"

interprateStmt (Print []) = ask
interprateStmt (Print (e:ex)) = do
  evalved <- evalExp e
  case evalved of
    (StorableBool b) -> liftIO $ print b
    (StorableInt int) -> liftIO $ print int
    (StorableString s) -> liftIO $ putStr s
    (UndeclaredBool) -> throwError $ "Value undeclared" ++ showEVar e
    (UndeclaredInt) -> throwError $ "Value undeclared" ++ showEVar e
    (UndeclaredString) -> throwError $ "Value undeclared" ++ showEVar e
    _ -> case e of 
      (EVar _) -> throwError $ "This is not printable: " ++ showEVar e
      _ -> throwError $ "Trying to print invalid value"
  interprateStmt (Print ex)
  ask

interprateStmt (For i e1 e2 st) = do
  env <- ask
  ev1 <- evalExp e1
  ev2 <- evalExp e2
  loc <- newloc
  validateLoopAssign st "For"
  case (ev1, ev2) of
    (StorableInt int1, StorableInt int2) -> do
      modify $ Map.insert loc (True, ev1)
      newEnv <- local (const $ Map.insert i loc env) (execForLoop i ev2 st)
      return env
    (_, _) -> throwError $ "For inputs must be the integer type" ++ showIdent i

interprateStmt (SExp e) = do
  evalExp e
  ask

interprateStmt (ArrAss i es e) = do
  env <- ask
  (_, arr) <- getVal i
  loc <- getLoc i
  evalved <- evalExp e
  l <- mapM evalExp es
  if checkList l
    then do
      let modedArr = modifiedArr arr l evalved
      let errorCheck = checkForErrorType modedArr emptyString
      if errorCheck == emptyString      
        then do
          modify $ Map.insert loc (False, modedArr)
          return env
        else
          throwError errorCheck
    else throwError $ "Array arguments must be the integer type" ++ showIdent i

execForLoop :: Ident -> Storable -> Stmt -> EnvState Env
execForLoop i (StorableInt i2) st = do
  env <- ask
  loc <- getLoc i
  (_, StorableBool breakOn) <- getVal (Ident "__breakState__")
  (_, StorableInt i1) <- getVal i
  if breakOn then do
    modify $ Map.insert breakKey (True, StorableBool False)
    return env
  else if i1 == i2
    then return env
    else do
      newEnv <- interprateStmt st
      modify $ Map.insert continueKey (True, StorableBool False)
      modify $ Map.insert loc (True, StorableInt (i1 + 1))
      local (const newEnv) (execForLoop i (StorableInt i2) st)

fillFunEnv :: Env -> [Arg] -> [Storable] -> EnvState Env
fillFunEnv fEnv [] [] = ask
fillFunEnv fEnv (Argum (SimpleType t) i : as) (e:es) = do
  env <- ask
  loc <- newloc
  if doesTypesMatch t e
    then do
      let newEnv = Map.insert i loc env
      modify $ Map.insert loc (False, e)
      local (const newEnv) (fillFunEnv fEnv as es)
    else
      throwError $ "Function arguments does not match types" ++ showIdent i

fillFunEnv fEnv (Argum (CollectionType (Array arrT)) i : as) (e:es) = do
  env <- ask
  loc <- newloc
  if arrTypesMatch arrT e
    then do
      let newEnv = Map.insert i loc env
      modify $ Map.insert loc (False, e)
      local (const newEnv) (fillFunEnv fEnv as es)
    else
      throwError $ "Function arguments does not match types" ++ showIdent i
  
evalExp :: Expr -> EnvState Storable
evalExp (ELitInt integer) = return (StorableInt integer)
evalExp ELitTrue = return (StorableBool True)
evalExp ELitFalse = return (StorableBool False)
evalExp (EString string) = return (StorableString string)

evalExp (EVar ident) = do
  (_, val) <- getVal ident
  return val

evalExp (EApp i es) = do
  env <- ask
  (_, StorableFun (fEnv, fType, args, bloc)) <- getVal i
  evalved <- mapM evalExp es
  if length evalved == length args
    then do
      newFEnv <- local (const fEnv) (fillFunEnv fEnv args evalved)
      local (const newFEnv) (interprateBlockStart bloc)
      (_, ret) <- getVal (Ident "__returnValue__")
      local (const env) ask
      if allTypesMatch fType ret
        then do
          modify $ Map.insert returnValueKey (True, Undefined)
          return ret
        else throwError $ "Function type does not match result type" ++ showIdent i
    else throwError $ "Missing arguments inside the function call" ++ showIdent i

evalExp (Not e) = do
  evaled <- evalExp e
  case evaled of
    (StorableBool True) -> return (StorableBool False)
    (StorableBool False) -> return (StorableBool True)
    _ -> throwError "Cannot use Not operateor on non boolean type"

evalExp (Neg e) = do
  evaled <- evalExp e
  case evaled of
    (StorableInt integer) -> return (StorableInt (-integer))
    _ -> throwError "Only integer type can be negated"

evalExp (EMul e1 mulOp e2) = do
  evaled1 <- evalExp e1
  evaled2 <- evalExp e2
  case (evaled1, evaled2) of
    (StorableInt int1, StorableInt int2) -> case mulOp of
      Times -> return $ StorableInt (int1 * int2)
      Div -> if int2 == 0
        then throwError "Cannot div by 0"
        else return $ StorableInt (int1 `div` int2)
      Mod -> if int2 == 0
        then throwError "Cannot mod by 0"
        else return $ StorableInt (int1 `mod` int2)
    (_, _) -> throwError "Mul OP types are not integers"
  
evalExp (EAdd e1 addOp e2) = do
  evaled1 <- evalExp e1
  evaled2 <- evalExp e2
  case (evaled1, evaled2) of
    (StorableInt int1, StorableInt int2) -> case addOp of
      Plus -> return $ StorableInt (int1 + int2)
      Minus -> return $ StorableInt (int1 - int2)
    (StorableString s1, StorableString s2) -> case addOp of
      Plus -> return $ StorableString (s1 ++ s2)
      Minus -> throwError "Cannot `minus` string"
    (_, _) -> throwError "Types of add operations do not match"

evalExp (ERel e1 relOp e2) = do
  evaled1 <- evalExp e1
  evaled2 <- evalExp e2
  case (evaled1, evaled2) of
    (StorableInt int1, StorableInt int2) -> case relOp of
      LTH -> return $ StorableBool (int1 < int2)
      LE -> return $ StorableBool (int1 <= int2)
      GTH -> return $ StorableBool (int1 > int2)
      GE -> return $ StorableBool (int1 >= int2)
      EQU -> return $ StorableBool (int1 == int2)
      NE -> return $ StorableBool (int1 /= int2)
      TR -> return $ StorableInt (trychotomy int1 int2)
    (StorableString s1, StorableString s2) -> case relOp of
      LTH -> return $ StorableBool (s1 < s2)
      LE -> return $ StorableBool (s1 <= s2)
      GTH -> return $ StorableBool (s1 > s2)
      GE -> return $ StorableBool (s1 >= s2)
      EQU -> return $ StorableBool (s1 == s2)
      NE -> return $ StorableBool (s1 /= s2)
      TR -> return $ StorableInt (trychotomy s1 s2)
    (StorableBool b1, StorableBool b2) -> case relOp of
      EQU -> return $ StorableBool (b1 == b2)
      NE -> return $ StorableBool (b1 /= b2)
      TR -> throwError "Boolean types does not corespond to trychotomy"
      _ -> return (StorableBool False)
    (_, _) -> throwError "Different types variables are not comperable"
    

evalExp (EAnd e1 e2) = do
  evaled1 <- evalExp e1
  evaled2 <- evalExp e2
  case (evaled1, evaled2) of
    (StorableBool True, StorableBool True) -> return (StorableBool True)
    (StorableBool True, StorableBool False) -> return (StorableBool False)
    (StorableBool False, StorableBool True) -> return (StorableBool False)
    (StorableBool False, StorableBool False) -> return (StorableBool False)
    (_, _) -> throwError "And operation can be only performed on boolean type"

evalExp (EOr e1 e2) = do
  evaled1 <- evalExp e1
  evaled2 <- evalExp e2
  case (evaled1, evaled2) of
    (StorableBool True, StorableBool True) -> return (StorableBool True)
    (StorableBool True, StorableBool False) -> return (StorableBool True)
    (StorableBool False, StorableBool True) -> return (StorableBool True)
    (StorableBool False, StorableBool False) -> return (StorableBool False)
    (_, _) -> throwError "Or operation can be only performed on boolean type"

evalExp (EArr i es) = do
  evalved <- mapM evalExp es
  (_, arr) <- getVal i
  if checkList evalved
    then case getArrVal arr evalved of
      (ErrorType err) -> throwError err
      res -> return res
    else throwError $ "Array parameter are not integer type" ++ showIdent i

