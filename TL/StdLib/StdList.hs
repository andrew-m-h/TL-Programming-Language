module StdLib.StdList (
	lengthTL,
	totalTL,
	productTL,
	elemTL,
	insertTL,
	unionTL,
	concatTL,
	sortTL,
	enumFromToTL
	) where

import Data.List (sort, insert, union)
import Token

lengthTL :: Expression -> IO(Token)
lengthTL [LiteralL lst] = return $ LiteralI $ fromIntegral $ length lst
lengthTL param = error $ "length called with incorrect parameters: " ++ show param

totalTL :: Expression -> IO(Token)
totalTL [LiteralL lst] = case lst of
	(LiteralI _):_ -> return $ LiteralI $ totalI lst
	(LiteralF _):_ -> return $ LiteralF $ totalF lst
	e:_ -> error $ "cannot call sum with: [" ++ show e ++ "...]"
	[] -> return $ LiteralI 0
	where
		totalI :: [Token] -> Integer
		totalI list = case list of
			[]	-> 0
			(LiteralI x):xs -> x + totalI xs
			e:_ -> error $ "cannot call totalI with: [" ++ show e ++ "...]"
		totalF :: [Token] -> Double
		totalF list = case list of
			[]	-> 0
			(LiteralF x):xs -> x + totalF xs
			e:_ -> error $ "cannot call totalF with: [" ++ show e ++ "...]"
totalTL param = error $ "sum called with incorrect parameters: " ++ show param

productTL :: Expression -> IO(Token)
productTL [LiteralL lst] = case lst of
	(LiteralI _):_ -> return $ LiteralI $ productI lst
	(LiteralF _):_ -> return $ LiteralF $ productF lst
	e:_ -> error $ "cannot call product with: [" ++ show e ++ "...]"
	[] -> return $ LiteralI 0
	where
		productI :: [Token] -> Integer
		productI list = case list of
			[] -> 1
			(LiteralI x):xs -> x * productI xs
			e:_ -> error $ "cannot call productI with: [" ++ show e ++ "...]"
		productF :: [Token] -> Double
		productF list = case list of
			[] -> 1
			(LiteralF x):xs -> x * productF xs
			e:_ -> error $ "cannot call productF with: [" ++ show e ++ "...]"
productTL param = error $ "product called with incorrect parameters: " ++ show param

sortTL :: Expression -> IO(Token)
sortTL [LiteralL lst] = return $ LiteralL $ sort lst
sortTL param = error $ "sort called with incorrect parameters: " ++ show param

elemTL :: Expression -> IO(Token)
elemTL [e, LiteralL lst] = return $ LiteralB $ e `elem` lst
elemTL param = error $ "member called with incorrect parameters: " ++ show param

insertTL :: Expression -> IO(Token)
insertTL [e, LiteralL lst] = return $ LiteralL $ insert e lst
insertTL param = error $ "insert called with incorrect parameters: " ++ show param

concatTL :: Expression -> IO(Token)
concatTL [LiteralL list] = case list of
	(LiteralL x):xs -> concatTL [LiteralL xs] >>= return . LiteralL . (x++) . fromLitL 
	[] -> return $ LiteralL []
	e:_ -> error $ "cannot call flatten with argument: [" ++ show e ++ "...]"
concatTL param = error $ "flatten called with incorrect parameters: " ++ show param

unionTL :: Expression -> IO(Token)
unionTL [LiteralL lst1, LiteralL lst2] = return $ LiteralL $ union lst1 lst2
unionTL param = error $ "union called with incorrect parameters: " ++ show param

enumFromToTL :: Expression -> IO(Token)
enumFromToTL [LiteralI start, LiteralI end] = return $ LiteralL $ map LiteralI [start..end]
enumFromToTL param = error $ "enumFromTo called with invalid arguments: " ++ show param