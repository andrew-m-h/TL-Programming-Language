module TL.ParseTL (
	tlDef,
	tlParser,
	parseProgram,
	parse,
	) where

import Control.Monad (return, (>>), (>>=))
import Data.Bool (Bool(..))
import Data.Function (($), (.))
import Data.List ((++))
import Data.String (String)
import Prelude (Integer, fromIntegral, error, floor, (**), (^), (+), (-), (*), (/), mod, div)
import TL.Token (Op(..), CodeBlock(..), Program(..), Token(..), Expression)
import Text.Parsec ((<|>), char, sepBy, try, letter, alphaNum, parserZero, many1, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Token
import Text.Show (Show, show)

tlDef :: LanguageDef ()
tlDef = LanguageDef {
	commentStart 	= "{-",
	commentEnd 		= "-}",
	commentLine 	= "#",
	nestedComments 	= True,
	identStart 		= letter <|> char '_',
	identLetter 	= alphaNum <|> char '_',
	opStart			= parserZero,
	opLetter 		= parserZero,
	reservedNames	= [
		"while",	"select", 	"for_each",
		"else",		"case",		"printLn",
		"if",		"delete",	"int",
		"import",	"bool",		"invoke",
		"return",	"def",		"getline",
		"for",	 	"True", 	"False",
		"print",	"str",		"std",
		"subroutine", "global",	"new"
	],
	reservedOpNames = [
		":", "+","-","*","/","^", "%", "=", "==",
		"!=", ">", "<", ">=", "<=", "and", "or",
		"not", "xor", "!!", "+=", "-=", "*="
	],
	caseSensitive 	= True
}

tlParser :: TokenParser ()
tlParser = makeTokenParser tlDef

parseAssignment :: Parser CodeBlock
parseAssignment = do
	var <- parseVariable
	op <- assign <|> plusEqual <|> minusEqual <|> timesEqual
	expr <- parseExpression
	case op of
		Assign -> return $ Assignment var expr
		PlusEqual -> return $ Assignment var [var, Operator Plus, Paren expr]
		MinusEqual -> return $ Assignment var [var, Operator Minus, Paren expr]
		TimesEqual -> return $ Assignment var [var, Operator Times, Paren expr]
		_ -> error $ "Assignment statment does not utilise operator: " ++ show op

parseNewAssignment :: Parser CodeBlock
parseNewAssignment = do
	_ <- reserved tlParser "new"
	var <- parseVariable
	_ <- assign
	expr <- parseExpression
	return $ NewAssignment var expr 

parseGlobalAssignment :: Parser CodeBlock
parseGlobalAssignment = do
	_ <- tlGlobal
	var <- parseVariable
	_ <- assign
	expr <- parseExpression
	return $ GlobalAssignment var expr

parseIfThen :: Parser CodeBlock
parseIfThen = do
	_ <- reserved tlParser "if"
	condition <- parens tlParser parseExpression
	prog <- braces tlParser parseProgram
	return $ IfThen condition prog

parseIfElse :: Parser CodeBlock
parseIfElse = do
	_ <- reserved tlParser "if"
	condition <- parens tlParser parseExpression
	prog1 <- braces tlParser parseProgram
	_ <- reserved tlParser "else"
	prog2 <- braces tlParser parseProgram
	return $ IfElse condition prog1 prog2

parseWhile :: Parser CodeBlock
parseWhile = do
	_ <- reserved tlParser "while"
	condition <- parens tlParser parseExpression
	prog <- braces tlParser parseProgram
	return $ While condition prog

parseFor :: Parser CodeBlock
parseFor = do
	_ <- reserved tlParser "for"
	[[var],sta,sto,ste] <- parens tlParser (sepBy parseExpression (comma tlParser))
	prog <- braces tlParser parseProgram 
	return $ For (var, sta, sto, ste) prog

parseForEach :: Parser CodeBlock
parseForEach = do
	_ <- reserved tlParser "for_each"
	var <- parseVariable
	_ <- reserved tlParser "in"
	struct <- parseExpression
	prog <- braces tlParser parseProgram
	return $ ForEach (var, struct) prog


parseSelect :: Parser CodeBlock
parseSelect = do
	reserved tlParser "select"
	parseSelect' >>= (return . Select)
		where
			parseSelect' :: Parser [(Expression, Program)]
			parseSelect' = do
				(try (reserved tlParser "case" >>
					do 
						cond <- parens tlParser parseExpression
						prog <- braces tlParser parseProgram
						parseSelect' >>= (return . ((cond, prog):))
						)) <|>
					return []

parseProgram :: Parser Program
parseProgram = 
	whiteSpace tlParser >> many1 parseLine >>= (return . Prog)

parseFunction :: Parser CodeBlock
parseFunction = do
	_ <- reserved tlParser "def"
	Variable name <- parseVariable
	args <- parens tlParser (sepBy parseVariable (comma tlParser))
	prog <- braces tlParser parseProgram
	return $ Function name args prog

parseSubroutine :: Parser CodeBlock
parseSubroutine = do
	_ <- reserved tlParser "subroutine"
	Variable name <- parseVariable
	args <- parens tlParser (sepBy parseVariable (comma tlParser))
	prog <- braces tlParser parseProgram
	return $ Subroutine name args prog

parseExpression :: Parser Expression
parseExpression = do
	many1 ( try parseKeyword <|> try parseOperator <|> try parseFnCall <|>
		try parseLiteralB <|> try parseLiteralF <|> try parseLiteralI <|>
		try parseLiteralS <|> try parseStdFnCall <|> try parseVariable <|>
		try ((parens tlParser) parseExpression >>= (return . Paren)) <|>
		try parseLiteralL <|> try parseLiteralLCons <|> parseSubroutineCall
		)

parseLine :: Parser CodeBlock
parseLine = do
	line <- try parseGlobalAssignment <|> try parseAssignment <|> try parseIfElse <|> try (parseExpression >>= (return . Expr)) <|>
		try parseIfThen <|> try parseWhile <|> try parseSelect <|> try parseFor <|> try parseFunction <|>
		try parseForEach <|> try parseSubroutine <|> try parseNewAssignment
	_ <- semi tlParser
	return line

parseKeyword :: Parser Token
parseKeyword = try tlPrint <|> try tlGetLine <|> try tlImport <|> try tlGlobal <|>
	try str <|> try int <|> try bool <|> try tlReturn <|> try tlPrintLn <|> try tlDelete

tlPrint, tlPrintLn, tlGetLine, tlImport, str, int, bool, tlReturn, tlDelete, tlGlobal :: Parser Token
tlPrint = (reserved tlParser "print") >> (return $ Keyword "print")
tlPrintLn = (reserved tlParser "printLn") >> (return $ Keyword "printLn")
tlGetLine = (reserved tlParser "getline") >> (return $ Keyword "getline")
tlImport = (reserved tlParser "import") >> (return $ Keyword "import")
str = (reserved tlParser "str") >> (return $ Keyword "str")
int = (reserved tlParser "int") >> (return $ Keyword "int")
bool = (reserved tlParser "bool") >> (return $ Keyword "bool")
tlReturn = (reserved tlParser "return") >> (return $ Keyword "return")
tlDelete = reserved tlParser "delete" >> (return $ Keyword "delete")
tlGlobal = reserved tlParser "global" >> (return  $ Keyword "global")

parseLiteral :: Parser Token
parseLiteral = try parseLiteralS <|> try parseLiteralI <|> try parseLiteralF <|>
	try parseLiteralB <|> try parseLiteralL <|> parseLiteralLCons

parseLiteralS, parseLiteralI, parseLiteralF, parseLiteralB,
 	parseLiteralL, parseLiteralLCons, parseFnCall, 
 	parseStdFnCall, parseSubroutineCall :: Parser Token
parseLiteralS = stringLiteral tlParser >>= (return . LiteralS)
parseLiteralI = natural tlParser >>= (return . LiteralI)
parseLiteralF = float tlParser >>= (return . LiteralF)
parseLiteralB = do
	((reserved tlParser) "True" >> return True)
		<|> ((reserved tlParser) "False" >> return False)
	>>= (return . LiteralB)
parseLiteralL = brackets tlParser (sepBy parseLiteral (comma tlParser)) >>= (return . LiteralL)
parseLiteralLCons = do
	_ <- char '['
	start <- parseExpression
	_ <- dot tlParser
	_ <- dot tlParser
	end <- parseExpression
	_ <- char ']'
	return $ StdFnCall "enumFromTo" [start, end]

parseFnCall = do
	Variable name <- parseVariable
	args <- parens tlParser (sepBy parseExpression (comma tlParser))
	return $ FnCall name args 

parseStdFnCall = do
	_ <- reserved tlParser "std"
	_ <- colon tlParser
	_ <- colon tlParser
	Variable name <- parseVariable
	args <- parens tlParser (sepBy parseExpression (comma tlParser))
	return $ StdFnCall name args

parseSubroutineCall = do
	_ <- reserved tlParser "invoke"
	Variable name <- parseVariable
	args <- parens tlParser (sepBy parseExpression (comma tlParser))
	return $ SubroutineCall name args

parseVariable :: Parser Token
parseVariable = identifier tlParser >>= (return . Variable)

genOpParser :: String -> Op -> Parser Op
genOpParser x y = (reservedOp tlParser) x >> return y

plus, minus, times, divide, power, tlMod,
	equal, notEqual, xor, tlOr, tlAnd,
	gt, lt, ge, le, from, plusEqual,
	minusEqual, timesEqual, assign :: Parser Op
plus = genOpParser "+" Plus
minus = genOpParser "-" Minus
times = genOpParser "*" Times
divide = genOpParser "/" Divide
power = genOpParser "**" Power
tlMod = genOpParser "%" Mod

equal = genOpParser "==" Equal
notEqual = genOpParser "!=" NotEqual
ge = genOpParser ">=" GreaterEqual
gt = genOpParser ">" Greater
le = genOpParser "<=" LessEqual
lt = genOpParser "<" Less
xor = (reserved tlParser) "xor" >> return Xor
tlOr = (reserved tlParser) "or" >> return Or
tlAnd = (reserved tlParser) "and" >> return And

from = genOpParser "!!" From

plusEqual = genOpParser "+=" PlusEqual
minusEqual = genOpParser "-=" MinusEqual
timesEqual = genOpParser "*=" TimesEqual
assign = genOpParser "=" Assign

parseOperator :: Parser Token
parseOperator = (plus <|> minus <|> try times <|> divide <|> power <|> tlMod <|>
	equal <|> notEqual <|> ge <|> gt <|> le <|> lt <|> xor <|> tlOr <|> tlAnd <|>
	from) >>= (return . Operator)
