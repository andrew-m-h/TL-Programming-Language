module StdLib.StdIO (
	readFileTL,
	writeFileTL,
	appendFileTL,
	getCharTL
	) where

import Token

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