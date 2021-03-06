{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintMyLang.
--   Generated by the BNF converter.

module PrintMyLang where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, dropWhile, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsMyLang

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i = \case
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt     _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsMyLang.Ident where
  prt _ (AbsMyLang.Ident i) = doc $ showString i

instance Print AbsMyLang.Program where
  prt i = \case
    AbsMyLang.Prog topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print AbsMyLang.TopDef where
  prt i = \case
    AbsMyLang.FnDef type_ id_ args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsMyLang.TopDef] where
  prt = prtList

instance Print AbsMyLang.Arg where
  prt i = \case
    AbsMyLang.Argum type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsMyLang.Arg] where
  prt = prtList

instance Print AbsMyLang.Block where
  prt i = \case
    AbsMyLang.Bloc stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [AbsMyLang.Stmt] where
  prt = prtList

instance Print AbsMyLang.Stmt where
  prt i = \case
    AbsMyLang.Empty -> prPrec i 0 (concatD [doc (showString ";")])
    AbsMyLang.BStmt block -> prPrec i 0 (concatD [prt 0 block])
    AbsMyLang.Decl type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    AbsMyLang.ConstDecl type_ items -> prPrec i 0 (concatD [doc (showString "const"), prt 0 type_, prt 0 items, doc (showString ";")])
    AbsMyLang.Ass id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsMyLang.ArrAss id_ exprs expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "["), prt 0 exprs, doc (showString "]"), doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsMyLang.Ret expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsMyLang.VRet -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    AbsMyLang.Cond expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsMyLang.CondElse expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    AbsMyLang.While expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsMyLang.For id_ expr1 expr2 stmt -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 id_, doc (showString "="), prt 0 expr1, doc (showString "to"), prt 0 expr2, doc (showString ")"), prt 0 stmt])
    AbsMyLang.Print exprs -> prPrec i 0 (concatD [doc (showString "print"), doc (showString "("), prt 0 exprs, doc (showString ")"), doc (showString ";")])
    AbsMyLang.Break -> prPrec i 0 (concatD [doc (showString "break")])
    AbsMyLang.Continue -> prPrec i 0 (concatD [doc (showString "continue")])
    AbsMyLang.SExp expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsMyLang.Item where
  prt i = \case
    AbsMyLang.NoInit id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsMyLang.Init id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])
    AbsMyLang.ArrayInit id_ exprs -> prPrec i 0 (concatD [prt 0 id_, doc (showString "["), prt 0 exprs, doc (showString "]")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsMyLang.Item] where
  prt = prtList

instance Print AbsMyLang.Type where
  prt i = \case
    AbsMyLang.SimpleType stype -> prPrec i 0 (concatD [prt 0 stype])
    AbsMyLang.CollectionType ctype -> prPrec i 0 (concatD [prt 0 ctype])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsMyLang.Type] where
  prt = prtList

instance Print AbsMyLang.CType where
  prt i = \case
    AbsMyLang.Array stype -> prPrec i 0 (concatD [prt 0 stype, doc (showString "[]")])

instance Print AbsMyLang.SType where
  prt i = \case
    AbsMyLang.Int -> prPrec i 0 (concatD [doc (showString "int")])
    AbsMyLang.Str -> prPrec i 0 (concatD [doc (showString "string")])
    AbsMyLang.Bool -> prPrec i 0 (concatD [doc (showString "bool")])
    AbsMyLang.Void -> prPrec i 0 (concatD [doc (showString "void")])

instance Print AbsMyLang.Expr where
  prt i = \case
    AbsMyLang.EVar id_ -> prPrec i 6 (concatD [prt 0 id_])
    AbsMyLang.ELitInt n -> prPrec i 6 (concatD [prt 0 n])
    AbsMyLang.ELitTrue -> prPrec i 6 (concatD [doc (showString "true")])
    AbsMyLang.ELitFalse -> prPrec i 6 (concatD [doc (showString "false")])
    AbsMyLang.EApp id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsMyLang.EString str -> prPrec i 6 (concatD [prt 0 str])
    AbsMyLang.EArr id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "["), prt 0 exprs, doc (showString "]")])
    AbsMyLang.Neg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsMyLang.Not expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsMyLang.EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsMyLang.EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsMyLang.ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsMyLang.EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsMyLang.EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsMyLang.Expr] where
  prt = prtList

instance Print AbsMyLang.AddOp where
  prt i = \case
    AbsMyLang.Plus -> prPrec i 0 (concatD [doc (showString "+")])
    AbsMyLang.Minus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print AbsMyLang.MulOp where
  prt i = \case
    AbsMyLang.Times -> prPrec i 0 (concatD [doc (showString "*")])
    AbsMyLang.Div -> prPrec i 0 (concatD [doc (showString "/")])
    AbsMyLang.Mod -> prPrec i 0 (concatD [doc (showString "%")])

instance Print AbsMyLang.RelOp where
  prt i = \case
    AbsMyLang.LTH -> prPrec i 0 (concatD [doc (showString "<")])
    AbsMyLang.LE -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsMyLang.GTH -> prPrec i 0 (concatD [doc (showString ">")])
    AbsMyLang.GE -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsMyLang.EQU -> prPrec i 0 (concatD [doc (showString "==")])
    AbsMyLang.NE -> prPrec i 0 (concatD [doc (showString "!=")])
    AbsMyLang.TR -> prPrec i 0 (concatD [doc (showString "<>")])

