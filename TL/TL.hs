import InterpretTL (interpret)
import ParseTL (parseProgram, parse)
import System.Environment (getArgs)
import CommandLine.Options
import Scoping

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
				ParseOnly -> putStrLn $ show $ scope []  $nameSpaceP "" p
				Interpret -> interpret $ scope [] $ nameSpaceP "" p