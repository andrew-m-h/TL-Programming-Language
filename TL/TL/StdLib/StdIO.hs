{-# LANGUAGE NoImplicitPrelude #-}

module TL.StdLib.StdIO (
	readFileTL,
	writeFileTL,
	appendFileTL,
	getCharTL
	) where

import Control.Monad (Monad(..))
import Data.Function (($), (.))
import Data.List ((++))
import Prelude (error)
import System.IO (IO, readFile, writeFile, appendFile, getChar)
import TL.Token (Token(..), Expression)
import Text.Show (show)

readFileTL :: Expression -> IO(Token)
readFileTL [LiteralS path] = readFile path >>= (return . LiteralS)
readFileTL param = error $ "readfile called with incorrect parameters: " ++ show param

writeFileTL :: Expression -> IO(Token)
writeFileTL [LiteralS path, LiteralS cont] = writeFile path cont >> return Null
writeFileTL param = error $ "writefile called with incorrect parameters: " ++ show param

appendFileTL :: Expression -> IO(Token)
appendFileTL [LiteralS path, LiteralS cont] = appendFile path cont >> return Null
appendFileTL param = error $ "appendfile called with incorrect parameters: " ++ show param

getCharTL :: Expression -> IO(Token)
getCharTL [] = getChar >>= (return . LiteralS . show)
getCharTL param = error $ "getchar called with incorrect parameters: " ++ show param
