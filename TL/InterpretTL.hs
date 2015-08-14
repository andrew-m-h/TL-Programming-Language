module InterpretTL (
	program,
	interpret,
	function,
	ifThen,
	ifElse,
	while,
	for,
	select,
	assignment,
	expression
	) where

import ParseTL
import Token
import qualified StdLib.Stdlib as Std
import Control.Monad.State
import qualified Data.Map as M

type Variables = M.Map String (Either Token (Expression, Program))
type TLInterpreter = StateT Variables IO Token

function :: String -> Expression -> Program -> TLInterpreter
function name args prog = modify (M.insert name (Right (args, prog))) >> return Null

ifThen::Expression -> Program -> TLInterpreter
ifThen cond prog = do 
	LiteralB b <- expression cond Null
	case b of
		True -> program prog >> return Null
		False -> return Null

ifElse::Expression -> Program -> Program -> TLInterpreter
ifElse cond t f = do
	LiteralB b <- expression cond Null
	case b of
		True -> program t
		False -> program f

while::Expression -> Program -> TLInterpreter
while cond prog = do
	LiteralB b <- expression cond Null
	case b of
		True -> program prog >> while cond prog
		False -> return Null

for :: (Token, Token, Token, Token) -> Program -> TLInterpreter
for (var,sta,sto,ste) prog = do
	modify (M.insert (fromVar var) (Left sta))
	_ <- program prog
	LiteralB cond <- expression [var, Operator Less, sto] Null
	if cond then
		expression [sta, Operator Plus, ste] Null >>= (\start -> for (var, start, sto,ste) prog)
	else 
		modify (M.delete $ fromVar var) >> return Null 

forEach :: (Token, Token) -> Program -> TLInterpreter
forEach (var, struct) prog = do
	case struct of
		LiteralL (x:xs) -> assignment var [x] >> program prog >> forEach (var, LiteralL xs) prog
		LiteralL []	-> modify (M.delete $ fromVar var) >> return Null
		LiteralS (x:xs) -> assignment var [LiteralS [x]] >> program prog >> forEach (var, LiteralS xs) prog
		LiteralS []	-> modify (M.delete $ fromVar var) >> return Null
		_ -> error $ "cannot for_each over " ++ show struct

select :: [(Expression, Program)] -> TLInterpreter 
select cases = case cases of 
	[]	-> return Null 
	(cond, prog):xs -> do 
		LiteralB b <- expression cond Null
		if b then
			program prog >> return Null
		else
			select xs

assignment::Token -> Expression -> TLInterpreter
assignment (Variable var) expr =
	expression expr Null >>= modify . M.insert var . Left >> return Null
assignment t expr = error $ "cannot perform assignment between: " ++ (show t) ++ " and " ++ (show expr)

evalOperator :: Expression -> Op -> Token -> TLInterpreter
evalOperator expr op arg = case op of
	From -> (expression expr Null) >>= (return . (!!) (fromLitL arg) . fromEnum . fromLitI)
	Not -> (expression expr Null) >>= return . LiteralB . not . fromLitB
	_	-> case arg of
		LiteralB b -> (expression expr Null) >>= (return . LiteralB . opToBool op b . fromLitB)
		LiteralI i
			| isBoolOp op -> (expression expr Null) >>= (return . LiteralB . opToIntB op i . fromLitI)
			| otherwise -> (expression expr Null) >>= (return . LiteralI . opToInt op i . fromLitI)
		LiteralF f
			| isBoolOp op -> (expression expr Null) >>= (return . LiteralB . opToFloatB op f . fromLitF)
			| otherwise -> (expression expr Null) >>= (return . LiteralF . opToFloat op f . fromLitF)
		LiteralS s
			| isBoolOp op -> (expression expr Null) >>= (return . LiteralB . opToStringB op s . fromLitS)
			| otherwise -> (expression expr Null) >>= (return . LiteralS . opToString op s . fromLitS)
		LiteralL l
			| op == Times -> (expression expr Null) >>= (return . LiteralL . (flip replicate) (head l) . fromEnum . fromLitI)
			| isBoolOp op -> (expression expr Null) >>= (return . LiteralB . opToListB op l . fromLitL)
			| otherwise -> (expression expr Null) >>= (return . LiteralL . opToList op l . fromLitL)
		LiteralM _ -> error "no operators defined for the IntMap datatype.\nUse Standard library functions instead."
		_ -> error "cannot evaluate expression"	

evalKeyword :: Expression -> String -> TLInterpreter
evalKeyword expr kword = case kword of
	"print" -> (expression expr Null >>= (liftIO . putStr . tokenToString)) >> return Null
	"printLn" -> expression expr Null >>= (liftIO . putStrLn . tokenToString) >> return Null
	"getline" -> (liftIO $ getLine >>= return . LiteralS) >>= expression expr
	"str" -> (expression expr Null) >>= (return . LiteralS . tokenToString)
	"int" -> (expression expr Null) >>= (return . LiteralI . tokenToInt)
	"bool" -> (expression expr Null) >>= (return . LiteralB . tokenToBool)
	"delete" -> do 
		let [Variable var] = expr
		modify (M.delete var) >> return Null
	"import" -> do
		text <- expression expr Null >>= (liftIO . readFile . fromLitS)
		case parse parseProgram "" text of
			Left _ -> fail "could not import specified file";
			Right prog -> program $ prog
	"global" -> expression expr Null
	_ -> error $ "keyword is not implamented in TL: " ++ show kword

expression :: Expression -> Token -> TLInterpreter
expression expr runAlong = case expr of
	[] -> return runAlong
	x:xs -> case x of
		Null -> expression xs runAlong
		LiteralB b -> expression xs (LiteralB b)
		LiteralF f -> expression xs (LiteralF f)
		LiteralI i -> expression xs (LiteralI i)
		LiteralS s -> expression xs (LiteralS s)
		LiteralL l -> expression xs (LiteralL l)
		LiteralM m -> expression xs (LiteralM m)
		Operator op -> evalOperator xs op runAlong
		Keyword kword -> evalKeyword xs kword
		Variable var -> do
			vars <- get
			case M.lookup var vars of
				Nothing -> fail ("variable undeclared " ++ var)
				Just (Left v) -> expression xs v
				Just (Right _) -> fail $ "function not variable" ++ var
		Paren e -> expression e Null >>= expression xs
		StdFnCall name in_args -> do
			args <- mapM (flip expression Null) in_args
			case M.lookup name Std.functions of
				Nothing -> error $ "std function not in standard libary: " ++ (show name)
				Just fn -> (liftIO $ fn args) >>= expression xs
		FnCall name in_args -> do
			args <- mapM (flip expression Null) in_args
			vars <- get
			case M.lookup name vars of
				Nothing -> error $ "function not found: " ++ (show name)
				Just (Left _) -> error "not a function"
				Just (Right (farg, prog)) -> do
					r <- assign farg args >> program prog
					put vars
					expression xs r
		SubroutineCall name in_args -> do
			args <- mapM (flip expression Null) in_args
			vars <- get
			case M.lookup name vars of
				Nothing -> error $ "subroutine not found: " ++ show name
				Just (Left _) -> error "not a subroutine"
				Just (Right (farg, prog)) -> do
					r <- assign farg args >> program prog
					expression xs r
assign :: Expression -> Expression -> TLInterpreter
assign args vars = case (args, vars) of
	(x:xs, y:ys) -> assignment x [y] >> assign xs ys
	([],[]) -> return Null
	_ -> error "incorect number of arguments"

program :: Program -> TLInterpreter
program (Prog prog) = case prog of
	[] -> return Null 
	x:xs -> case x of
		Function name args prog1 -> function name args prog1 >> program (Prog xs)
		Subroutine name args prog1 -> function name args prog1 >> program (Prog xs)
		Assignment cond prog1 -> assignment cond prog1 >> program (Prog xs)
		NewAssignment cond prog1 -> assignment cond prog1 >> program (Prog xs)
		GlobalAssignment cond prog1 -> assignment cond prog1 >> program (Prog xs)
		IfThen cond prog1 -> ifThen cond prog1 >> program (Prog xs)
		IfElse cond prog1 prog2 -> ifElse cond prog1 prog2 >> program (Prog xs) 
		While cond prog1 -> while cond prog1 >> program (Prog xs) 
		Select list -> select list >> program (Prog xs)
		Expr (Keyword "return":rest) -> expression rest Null
		Expr [Keyword "delete", Variable var] -> modify (M.delete var) >> program (Prog xs)
		Expr expr -> expression expr Null >> program (Prog xs)
		For  (var,sta,sto,ste) prog1 -> do
			start <- expression sta Null
			stop <- expression sto Null
			step <- expression ste Null
			for (var, start, stop, step) prog1 >> program (Prog xs)
		ForEach (var,expr) prog1 -> do
			struct <- expression expr Null
			forEach (var, struct) prog1 >> program (Prog xs)

interpret :: Program -> IO()
interpret prog = evalStateT (program prog) M.empty >> return ()