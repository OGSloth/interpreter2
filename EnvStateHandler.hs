module EnvStateHandler where

import Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsMyLang

type Loc = Int

type EnvState = ReaderT Env (StateT MyState (ExceptT String IO))

data Storable = Undefined
      | UndeclaredBool
      | UndeclaredInt
      | UndeclaredString
      | StorableBool Bool
      | StorableArr [Storable]
      | StorableTuple [Storable]
      | StorableInt Integer
      | StorableString String
      | StorableVoid
      | StorableFun (Env, Type, [Arg], Block)
      | ErrorType String
      deriving (Show, Eq, Ord, Read)

type Env = Map.Map Ident Loc
type MyState = Map.Map Loc (Bool, Storable)

breakKey :: Loc
breakKey = 0

continueKey :: Loc
continueKey = 1

returnValueKey :: Loc
returnValueKey = 2

maxEnvConstLoc :: Loc
maxEnvConstLoc = 2

initialState :: MyState
initialState = Map.fromList [(breakKey, (True, StorableBool False)), (continueKey, (True, StorableBool False)), (returnValueKey, (True, Undefined))]

initialEnv :: Env
initialEnv = Map.fromList [(Ident "__breakState__", breakKey), (Ident "__continueState__", continueKey), (Ident "__returnValue__", returnValueKey)]

functionsMeetUp :: EnvState Env
functionsMeetUp = do
  env <- ask
  store <- get
  local (const env) (setFunctionEnv (toList store))

setFunctionEnv :: [(Loc, (Bool, Storable))] -> EnvState Env
setFunctionEnv [] = ask
setFunctionEnv ((loc, (flag, StorableFun (_, typ, args, bloc))) : fs) = do
  env <- ask
  modify $ Map.insert loc (flag, StorableFun (env, typ, args, bloc))
  local (const env) (setFunctionEnv fs)

setFunctionEnv (_:fs) = setFunctionEnv fs
  

showIdent (Ident a) = " (Variable: " ++ a ++ ")"
showEVar (EVar i) = showIdent i

allTypesMatch (SimpleType t1) = doesTypesMatch t1

doesTypesMatch t1 t2 = case (t1, t2) of
  (Int, StorableInt _) -> True
  (Str, StorableString _) -> True
  (Bool, StorableBool _) -> True
  (Void, StorableVoid) -> True
  (_, _) -> False

doesAssTypesMatch t1 t2 = case (t1, t2) of
  (StorableInt _, StorableInt _) -> True
  (StorableInt _, UndeclaredInt) -> True
  (StorableBool _, StorableBool _) -> True
  (StorableBool _, UndeclaredBool) -> True
  (StorableString _, StorableString _) -> True
  (StorableString _, UndeclaredString) -> True
  (_, _) -> False

validateIdent :: Ident -> EnvState ()
validateIdent i = do
  env <- ask
  state <- get
  case Map.lookup i env of
    Just loc -> do
      throwError $ "Already declared" ++ showIdent i
    Nothing -> return ()


newloc :: EnvState Loc
newloc = do
  state <- get
  case Map.maxViewWithKey state of
    Just ((k, _), _) -> return (k + 1)
    Nothing -> return 0


getVal :: Ident -> EnvState (Bool, Storable)
getVal i = do
  env <- ask
  state <- get
  case Map.lookup i env of
    Just loc ->
      case Map.lookup loc state of
        Just val -> return val
        Nothing -> throwError $ show "No val"
    Nothing ->
      throwError $ show "Undeclared val"

getLoc :: Ident -> EnvState Loc
getLoc i = do
  env <- ask
  case Map.lookup i env of
    Just loc -> return loc
    Nothing -> throwError $ show "Undeclared var"

isReturnOnF :: EnvState Bool
isReturnOnF = do
  (_, val) <- getVal (Ident "__returnValue__")
  return (val /= Undefined)
