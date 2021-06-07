module Utils where

import Data.Map as Map
import Data.List

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsMyLang
import EnvStateHandler

emptyString :: String
emptyString = ""

trychotomy t1 t2
  | t1 > t2 = 1
  | t1 == t2 = 0
  | otherwise = -1

checkList :: [Storable] -> Bool
checkList [] = True
checkList (StorableInt e : es) = e >= 0 && checkList es
checkList _ = False

mapSTypeToStorable :: SType -> Storable
mapSTypeToStorable x = case x of
  Int -> UndeclaredInt
  Str -> UndeclaredString
  Bool -> UndeclaredBool
  Void -> ErrorType "Can't use void as type"

getArrVal :: Storable -> [Storable] -> Storable
getArrVal arr [] = arr
getArrVal arr (StorableInt int : es) =
  case arr of 
    (StorableArr l) -> 
      if length l <= fromIntegral int
        then ErrorType "Out of bound array"
        else getArrVal (l !! fromIntegral int) es
    val -> val

checkForErrorType :: Storable -> String -> String
checkForErrorType (StorableArr arr) acc = concatMap (`checkForErrorType` acc) arr
checkForErrorType val acc = case val of
  (ErrorType s) -> acc ++ s ++ "\n"
  _ -> ""

modifiedArr :: Storable -> [Storable] -> Storable -> Storable
modifiedArr currVal [] val = if doesAssTypesMatch val currVal 
  then val
  else ErrorType "Missed matcht types in array assign"

modifiedArr arr (StorableInt int : es) val =
  case arr of 
    (StorableArr l) -> 
      if length l <= fromIntegral int
        then ErrorType "Out of bound array"
        else StorableArr $ Prelude.take i l ++ [modifiedArr (l !! i) es val] ++ Prelude.drop (i + 1) l
          where i = fromIntegral int
    _ -> ErrorType "Out of dimenstion array"

areArgsUnique args = length args == length (nub $ getArgsIdents args)

getArgsIdents args = getArgsIdentsFoo args []

getArgsIdentsFoo [] acc = acc
getArgsIdentsFoo ((Argum _ (Ident i)):args) acc = getArgsIdentsFoo args (i:acc)

validArgsTypes ((Argum (SimpleType Void) _):_) = False
validArgsTypes (_:args) = True && validArgsTypes args
validArgsTypes [] = True

