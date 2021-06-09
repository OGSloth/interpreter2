module EnvStateHandler where

import Data.Map as Map
import Data.List as L

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

doesTypesMatch2 t1 t2 = case (t1, t2) of
  (Int, StorableInt _) -> True
  (Int, UndeclaredInt) -> True
  (Str, StorableString _) -> True
  (Str, UndeclaredString) -> True
  (Bool, StorableBool _) -> True
  (Bool, UndeclaredBool) -> True
  (_, _) -> False

arrTypesMatch t1 t2 =
  case (t1, t2) of
    (t12, StorableArr t) -> arrTypesMatch t12 (head t)
    (t12, t22) -> doesTypesMatch t12 t22

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

validateLoopAssign :: Stmt -> String -> EnvState ()
validateLoopAssign (Decl _ _) loopType =
  throwError $ "Cannot declare variables inside the non block: " ++ loopType ++ " statement"
validateLoopAssign (ConstDecl _ _) loopType =
  throwError $ "Cannot declare variables inside the non block: " ++ loopType ++ " statement"
validateLoopAssign _ _ = return ()


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
      throwError $ "Undeclared val" ++ showIdent i

getLoc :: Ident -> EnvState Loc
getLoc i = do
  env <- ask
  case Map.lookup i env of
    Just loc -> return loc
    Nothing -> throwError $ "Undeclared var" ++ showIdent i

isReturnOnF :: EnvState Bool
isReturnOnF = do
  (_, val) <- getVal (Ident "__returnValue__")
  return (val /= Undefined)

validateBlock :: Block -> EnvState ()
validateBlock (Bloc b) = do
  case getDuplicates (validateBlockFoo b []) [] [] of
    [] -> return ()
    l -> throwError $ "Following variables are declared more than once: " ++ (asList l)

asList :: [String] -> String
asList ss = (L.intercalate "," ss)

memberL :: (Eq a) => a -> [a] -> Bool
memberL x [] = False
memberL x (y:ys) | x == y = True
                | otherwise = memberL x ys

getDuplicates :: [String] -> [String] -> [String] -> [String]
getDuplicates [] _ non = non
getDuplicates (i:is) prev non = 
  case (memberL i prev, memberL i is, memberL i non) of 
    (True, _, False) -> getDuplicates is (i:prev) (i:non)
    (_, True, False) -> getDuplicates is (i:prev) (i:non)
    (_, _, _) -> getDuplicates is (i:prev) non

validateBlockFoo :: [Stmt] -> [String] -> [String]
validateBlockFoo [] acc = acc
validateBlockFoo ((Decl _ inits):ss) acc = validateBlockFoo ss ((getIdents inits) ++ acc)
validateBlockFoo ((ConstDecl _ inits):ss) acc = validateBlockFoo ss ((getIdents inits) ++ acc)
validateBlockFoo (_:ss) acc = validateBlockFoo ss acc

getIdents :: [Item] -> [String]
getIdents i = getIdentsFoo i []

getIdentsFoo :: [Item] -> [String] -> [String]
getIdentsFoo [] acc = acc
getIdentsFoo ((NoInit (Ident i)):is) acc = getIdentsFoo is (i:acc)
getIdentsFoo ((Init (Ident i) _):is) acc = getIdentsFoo is (i:acc)
getIdentsFoo ((ArrayInit (Ident i) _):is) acc = getIdentsFoo is (i:acc)

