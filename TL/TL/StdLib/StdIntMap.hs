module TL.StdLib.StdIntMap (
    intMapTL,
    memberTL,
    lookupTL,
    insertTL,
    deleteTL,
    sizeTL,
    unionTL,
    sumTL,
    productTL
    ) where

import           Control.Monad (Monad(..))
import           Data.Function (($))
import           Data.Int (Int)
import qualified Data.IntMap as IM
import           Data.List ((++), head, map, foldr)
import           Data.Maybe (Maybe(..))
import           Prelude ( Integer, fromIntegral, fromEnum, error, (*), (+))
import           System.IO (IO)
import           TL.Token (Token(..), Expression, fromLitI)
import           Text.Show (show)

sumTL :: Expression -> IO(Token)
sumTL [LiteralM m] = case lst of
    (_, LiteralI _):_ -> return $ foldr addI (LiteralI 0) lst
    (_, LiteralF _):_ -> return $ foldr addF (LiteralF 0) lst
    [] -> return $ LiteralI 0
    x:_ -> error $ "sum called with non Numeric Intmap: [" ++ show x ++ "...]"
    where
        lst :: [(Int, Token)]
        lst = IM.toList m
        addI :: (Int, Token) -> Token -> Token
        addI (_, LiteralI a) (LiteralI b) = LiteralI $ a + b
        addI (_, e) _ = error $ "addI called with invalid parameter: " ++ show e
        addF :: (Int, Token) -> Token -> Token
        addF (_, LiteralF a) (LiteralF b) = LiteralF $ a + b
        addF (_, e) _ = error $ "addF called with invalid parameter: " ++ show e
sumTL param = error $ "sum called with invalid parameters: " ++ show param

productTL :: Expression -> IO(Token)
productTL [LiteralM m] = case lst of
    (_, LiteralI _):_ -> return $ foldr mulI (LiteralI 0) lst
    (_, LiteralF _):_ -> return $ foldr mulF (LiteralF 0) lst
    [] -> return $ LiteralI 0
    x:_ -> error $ "product called with non Numeric Intmap: [" ++ show x ++ "...]"
    where
        lst :: [(Int, Token)]
        lst = IM.toList m
        mulI :: (Int, Token) -> Token -> Token
        mulI (_, LiteralI a) (LiteralI b) = LiteralI $ a * b
        mulI (_, e) _ = error $ "mulI called with invalid parameter: " ++ show e
        mulF :: (Int, Token) -> Token -> Token
        mulF (_, LiteralF a) (LiteralF b) = LiteralF $ a * b
        mulF (_, e) _ = error $ "mulF called with invalid parameter: " ++ show e
productTL param = error $ "product called with invalid parameters: " ++ show param

intMapTL :: Expression -> IO(Token)
intMapTL [LiteralL lst] = case lst of
    (LiteralL [LiteralI _, _]):_ -> return $ LiteralM $ IM.fromList $ map (\(LiteralL [a,b]) -> (fromEnum $ fromLitI a, b)) lst
    [] -> return $ LiteralM IM.empty
    _ -> error $ "key must be an Int: [" ++ show (head lst) ++ "...]"
intMapTL e = error $ "must call intmap with list of [int, val]: " ++ show e

memberTL :: Expression -> IO(Token)
memberTL [LiteralI key, LiteralM m] = return $ LiteralB $ IM.member (fromEnum key) m
memberTL e = error $ "must call member with Int and IntMap datatypes: " ++ show e

lookupTL :: Expression -> IO(Token)
lookupTL [LiteralI key, LiteralM m] = case IM.lookup (fromEnum key) m of
    Nothing -> error $ "key not in IntMap: " ++ show key
    Just val -> return val
lookupTL e = error $ "must call lookup with Int and IntMap: " ++ show e

insertTL :: Expression -> IO(Token)
insertTL [LiteralI key, val, LiteralM m] = return $ LiteralM $ IM.insert (fromEnum key) val m
insertTL e = error $ "must call insert with Int, value and IntMap: " ++ show e

deleteTL :: Expression -> IO(Token)
deleteTL [LiteralI key, LiteralM m] = return $ LiteralM $ IM.delete (fromEnum key) m
deleteTL e = error $ "must call delete with Int and IntMap: " ++ show e

sizeTL :: Expression -> IO(Token)
sizeTL [LiteralM m] = return $ LiteralI $ fromIntegral $ IM.size m
sizeTL e = error $ "must call size with IntMap: " ++ show e

unionTL :: Expression -> IO(Token)
unionTL [LiteralM m1, LiteralM m2] = return $ LiteralM $ IM.union m1 m2
unionTL e = error $ "must call union with IntMap & IntMap: " ++ show e
