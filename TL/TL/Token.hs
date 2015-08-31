{-# LANGUAGE NoImplicitPrelude #-}

module TL.Token (
	Op(..),
	CodeBlock(..),
	Program(..),
	Token(..),
	Expression,

	tokenToString,
	tokenToInt,
	tokenToFloat,
	tokenToBool,

	opToBool,
	opToInt,
	opToIntB,
	opToFloat,
	opToFloatB,
	opToString,
	opToStringB,
	opToList,
	opToListB,
	isBoolOp,

	fromLitB,
	fromLitI,
	fromLitF,
	fromLitS,
	fromLitL,
	fromVar,

	fmapP,
	fmapC
	)where

import           Data.Bool (Bool, not, (||), (&&))
import           Data.Eq (Eq, (==), (/=))
import           Data.Function (($), flip)
import qualified Data.IntMap as IM
import           Data.List ((++), head, elem, drop, map, null)
import           Data.Ord (Ord, Ordering(..), (>), (<), (>=), (<=), compare)
import           Data.String (String)
import           GHC.Exts (Double)
import           Prelude (Integer, fromIntegral, error, floor, (**), (^), (+), (-), (*), (/), mod, div)
import           Text.Read (read)
import           Text.Show (Show, show)

data Token =
	Null |
	LiteralS String |
	LiteralI Integer |
	LiteralF Double |
	LiteralB Bool |
	LiteralL Expression |
	LiteralM (IM.IntMap Token)|
	Keyword String |
	Operator Op |
	Variable String |
	FnCall String [Expression] |
	StdFnCall String [Expression] |
	SubroutineCall String [Expression] |
	Paren Expression
	deriving (Eq)

data Op = Plus | Minus | Times | Divide | Power | Mod |
	Assign | Equal | NotEqual | Not | Or | And | Xor | Negate |
	GreaterEqual | Greater | LessEqual | Less | From | PlusEqual |
	MinusEqual | TimesEqual
	deriving (Eq, Show)

data CodeBlock =
	IfThen Expression Program |
	IfElse Expression Program Program |
	While  Expression Program |
	For (Token, Expression, Expression, Expression) Program | --start, stop, step
	ForEach (Token, Expression) Program |
	Select [(Expression, Program)] |
	Assignment Token Expression |
	NewAssignment Token Expression |
	GlobalAssignment Token Expression |
	Function String Expression Program |
	Subroutine String Expression Program |
	Expr Expression
	deriving (Eq, Show)

newtype Program = Prog [CodeBlock]
	deriving (Eq, Show)

fmapP :: (Expression -> Expression) -> Program -> Program
fmapP f (Prog prog) = Prog $ map (fmapC f) prog

fmapC :: (Expression -> Expression) -> CodeBlock -> CodeBlock
fmapC f prog = case prog of
	IfThen exp1 prog1 -> IfThen (f exp1) (fmapP f prog1)
	IfElse exp1 prog1 prog2 -> IfElse (f exp1) (fmapP f prog1) (fmapP f prog2)
	While exp1 prog1 -> While (f exp1) (fmapP f prog1)
	For (t1, exp1, exp2, exp3) prog1 -> For (head $ f [t1], f exp1, f exp2, f exp3) (fmapP f prog1)
	ForEach (t1, exp1) prog1 -> ForEach (head $ f [t1], f exp1) (fmapP f prog1)
	Select lst -> Select $ map (\(exp1, prog1) -> (f exp1, fmapP f prog1)) lst 
	Assignment t1 exp1 -> Assignment (head $ f [t1]) (f exp1)
	NewAssignment t1 exp1 -> NewAssignment (head $ f [t1]) (f exp1)
	GlobalAssignment t1 exp1 -> GlobalAssignment (head $ f [t1]) (f exp1)
	Function str exp1 prog1 -> Function str (f exp1) (fmapP f prog1)
	Subroutine str exp1 prog1 -> Subroutine str (f exp1) (fmapP f prog1)
	Expr exp1 -> Expr (f exp1)

type Expression = [Token]

instance Show Token where
	show t = case t of
		Null -> "null"
		LiteralS s -> s
		LiteralI i -> show i
		LiteralF f -> show f 
		LiteralB b -> show b
		LiteralL l -> show l 
		LiteralM m -> drop 9 $ show m
		Keyword s -> show s 
		Operator op -> show op 
		Variable v -> show v 
		Paren e -> show e
		FnCall name vars -> name ++ " (" ++ show vars ++ ")"
		StdFnCall name vars -> "std::" ++ name ++ " (" ++ show vars ++ ")"
		SubroutineCall name vars -> "invoke " ++ name ++ " (" ++ show vars ++ ")"

instance Ord Token where
	compare a b = case (a, b) of
		(LiteralI a', LiteralI b') -> compare a' b'
		(LiteralF a', LiteralF b') -> compare a' b'
		(LiteralS a', LiteralS b') -> compare a' b'
		(LiteralB a', LiteralB b') -> compare a' b'
		_ -> EQ

opToBool :: Op -> (Bool -> Bool -> Bool)
opToBool op = case op of
	And -> (&&)
	Or -> (||)
	Xor -> (/=)
	Equal -> (==)
	NotEqual -> (/=)
	_ -> error $ "Operator cannot be used here: " ++ show op

opToInt :: Op -> (Integer -> Integer -> Integer)
opToInt op = case op of
	Plus -> (+)
	Minus -> (-)
	Times -> (*)
	Divide -> div
	Power -> (^)
	Mod -> mod
	_ -> error $ "Operator cannot be used here: " ++ show op

opToIntB :: Op -> (Integer -> Integer -> Bool)
opToIntB op = case op of
	Equal -> (==)
	NotEqual -> (/=)
	Greater -> (>)
	Less -> (<)
	GreaterEqual -> (>=)
	LessEqual -> (<=)
	_ -> error $ "Operator cannot be used here: " ++ show op

opToFloat :: Op -> (Double -> Double -> Double)
opToFloat op = case op of
	Plus -> (+)
	Minus -> (-)
	Times -> (*)
	Divide -> (/)
	Power -> (**)
	_ -> error $ "Operator cannot be used here: " ++ show op

opToFloatB :: Op -> (Double -> Double -> Bool)
opToFloatB op = case op of
	Equal -> (==)
	NotEqual -> (/=)
	Greater -> (>)
	Less -> (<)
	GreaterEqual -> (>=)
	LessEqual -> (<=)
	_ -> error $ "Operator cannot be used here: " ++ show op

opToString :: Op -> (String -> String -> String)
opToString op = case op of
	Plus -> (++)
	_ -> error $ "Operator cannot be used here: " ++ show op
	
opToStringB :: Op -> (String -> String -> Bool)
opToStringB op = case op of
	Equal -> (==)
	NotEqual -> (/=)
	_ -> error $ "Operator cannot be used here: " ++ show op

opToList :: (Eq a) => Op -> ([a] -> [a] -> [a])
opToList op = case op of
	Plus -> (++)
	_ -> error $ "Operator cannot be used here: " ++ show op

opToListB :: (Eq a) => Op -> ([a] -> [a] -> Bool)
opToListB op = case op of 
	Equal -> (==)
	NotEqual -> (/=)
	_ -> error $ "Operator cannot be used here: " ++ show op

isBoolOp::Op -> Bool
isBoolOp = flip elem [
	Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual]

tokenToString :: Token -> String
tokenToString = show

tokenToBool :: Token -> Bool
tokenToBool t = case t of
	LiteralB b -> b
	LiteralI i -> i /= 0
	LiteralF f -> f /= 0.0
	LiteralS s -> not $ null s
	_ -> error "cannot convert token to bool"

tokenToFloat :: Token -> Double
tokenToFloat t = case t of
	LiteralI i -> fromIntegral i
	LiteralF f -> f
	_ -> error "cannot convert token to float"

tokenToInt :: Token -> Integer 
tokenToInt t = case t of
	LiteralI i -> i
	LiteralF f -> floor f
	LiteralS s -> read s
	LiteralB b -> if b then 1 else 0
	_ -> error "cannot convert token to int"

fromLitB :: Token -> Bool
fromLitB lit = case lit of
	LiteralB b -> b
	_ -> error $ "Not literalB: " ++ show lit

fromLitI :: Token -> Integer 
fromLitI lit = case lit of
	LiteralI i -> i
	_ -> error $ "Not LiteralI: " ++ show lit

fromLitF :: Token -> Double 
fromLitF lit = case lit of
	LiteralF f -> f
	_ -> error $ "Not LiteralF: " ++ show lit 

fromLitS :: Token -> String 
fromLitS lit = case lit of 
	LiteralS r -> r
	_ -> error $ "Not LiteralS: " ++ show lit 

fromLitL :: Token -> Expression
fromLitL lit = case lit of
	LiteralL expr -> expr
	_ -> error $ "Not LiteralL: " ++ show lit

fromVar :: Token -> String
fromVar var = case var of
	Variable v  -> v
	_ -> error $ "Not Variable: " ++ show var
