{-# LANGUAGE NoImplicitPrelude #-}

module TL.Scoping (
	nameSpaceP,
	scope
) where

import Data.Function (($))
import Data.List ((++), map)
import Data.String (String)
import Prelude (Integer,fromIntegral, error, floor, (**), (^), (+), (-), (*), (/), mod, div)
import TL.Token (Token(..), Expression, CodeBlock(..), Program(..))

scope :: [Token] -> Program -> Program
scope vars (Prog prog) = case prog of
	[] -> Prog $ delVar vars
	x:xs -> case x of
		IfThen expr1 prog1 -> IfThen expr1 (sc prog1)|>sc (Prog xs)
		IfElse expr1 prog1 prog2 -> IfElse expr1 (sc prog1) (sc prog2)|>sc (Prog xs)
		While expr1 prog1 -> While (expr1) (sc prog1)|>sc (Prog xs)
		For expr prog1 -> For expr (sc prog1)|>sc (Prog xs)
		ForEach expr prog1 -> ForEach expr (sc prog1)|>sc (Prog xs)
		Select lst -> (Select $ map (\(expr1, prog1) -> (expr1, sc prog1)) lst)|>sc (Prog xs)
		Assignment t1 expr1 -> Assignment t1 expr1|>sc (Prog xs)
		NewAssignment t1 expr1 -> NewAssignment t1 expr1|>scope (t1:vars) (Prog xs)
		GlobalAssignment t1 expr1 -> GlobalAssignment t1 expr1 |> sc (Prog xs)
		Function str1 expr1 prog1 -> Function str1 expr1 (sc prog1)|>sc (Prog xs)
		Subroutine str1 expr1 prog1 -> Subroutine str1 expr1 (sc prog1)|>sc (Prog xs)
		Expr expr1 -> Expr expr1|>sc (Prog xs)
	where
		sc = scope []
		delVar :: [Token] -> [CodeBlock]
		delVar lst = case lst of
				[] -> []
				x:xs -> Expr [Keyword "delete", x]:delVar xs

nameSpaceP :: String -> Program -> Program
nameSpaceP name (Prog prog) = case prog of
	[] -> Prog []
	x:xs -> case x of
		IfThen expr1 prog1 -> IfThen (ne expr1) (np prog1)|>np (Prog xs)
		IfElse expr1 prog1 prog2 -> IfElse (ne expr1) (np prog1) (np prog2)|>np (Prog xs)
		While expr1 prog1 -> While (ne expr1) (np prog1)|>np (Prog xs)
		For (t1, expr1,expr2,expr3) prog1 -> For (nt t1, ne expr1, ne expr2, ne expr3) (np prog1)|>np (Prog xs)
		ForEach (t1, expr1) prog1 -> ForEach (nt t1, ne expr1) (np prog1)|>np (Prog xs)
		Select lst -> (Select $ map (\(expr1, prog1) -> (ne expr1, np prog1)) lst)|>np (Prog xs)
		Assignment t1 expr1 -> Assignment (nt t1) (ne expr1)|>np (Prog xs)
		NewAssignment t1 expr1 -> NewAssignment (nt t1) (ne expr1)|>np (Prog xs)
		GlobalAssignment t1 expr1 -> GlobalAssignment t1 (ne expr1)|>np (Prog xs)
		Function str1 expr1 prog1 -> Function str1 (ne expr1) (np prog1)|>np (Prog xs)
		Subroutine str1 expr1 prog1 -> Subroutine str1 (nameSpaceE (str1++name) expr1) (nameSpaceP (str1++name) prog1)|>np (Prog xs)
		Expr expr1 -> Expr (ne expr1)|>np (Prog xs)
		where
			np = nameSpaceP name
			ne = nameSpaceE name 
			nt = nameSpaceT name

nameSpaceE :: String -> Expression -> Expression
nameSpaceE name expr = case expr of
	[] -> []
	Keyword "global":Variable var:xs -> Variable var:nameSpaceE name xs
	x:xs -> nameSpaceT name x:nameSpaceE name xs 

nameSpaceT :: String -> Token -> Token
nameSpaceT name token = case token of
	Variable str -> Variable (name++str)
	FnCall str1 lst -> FnCall str1 (map (nameSpaceE name) lst)
	StdFnCall str1 lst -> StdFnCall str1 (map (nameSpaceE name) lst)
	SubroutineCall str1 lst -> SubroutineCall str1 (map (nameSpaceE name) lst)
	Paren expr -> Paren (nameSpaceE name expr)
	_ -> token

(|>) :: CodeBlock -> Program -> Program
(|>) c (Prog p) = Prog (c:p)
