{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Applicative
import           Control.Monad          (Monad (..))
import           Data.Either            (Either (..))
import           Data.Function          (($))
import           Prelude                (error)
import           System.Environment     (getArgs)
import           System.IO              (IO, getLine, putStrLn, readFile)
import           Text.Show              (show)
import           TL.CommandLine.Options (Action (..), optAction, optHelp,
                                         optVersion, tlHelp, tlOptions,
                                         tlVersion)
import           TL.InterpretTL         (interpret)
import           TL.ParseTL             (parse, parseProgram)
import           TL.Scoping             (nameSpaceP, scope)

test :: IO()
test = do
	prog <- getLine >>= readFile
	case parse parseProgram "" prog of
		Left _ -> error "could not parse program"
		Right p -> interpret $ scope [] $ nameSpaceP "" p

main :: IO()
main = do
  
	argv <- getArgs
	(opts, fname) <- tlOptions argv
	prog <- readFile fname
	if optVersion opts then
		putStrLn $ show $ tlVersion
	else if optHelp opts then
		putStrLn $ tlHelp
	else
		case parse parseProgram "" prog of
			Left _ -> fail "could not parse program"
			Right p -> case (optAction opts) of
				ParseOnly -> putStrLn $ show $ scope [] $ nameSpaceP "" p
				Interpret -> interpret $ scope [] $ nameSpaceP "" p
