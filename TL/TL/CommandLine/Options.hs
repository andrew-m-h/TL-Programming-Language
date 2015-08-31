{-# LANGUAGE NoImplicitPrelude #-}

module TL.CommandLine.Options (
	Version(..),
	Action(..),
	Options(..),

	defaultOptions,
	options,
	tlOptions,
	tlVersion,
	tlHelp
) where

import Control.Monad (Monad(..))
import Data.Bool (Bool(..))
import Data.Eq (Eq)
import Data.Function (($), id, flip)
import Data.Int (Int)
import Data.List ((++), concat, foldl)
import Prelude (String)
import System.Console.GetOpt (usageInfo, OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt)
import System.IO (IO)
import System.IO.Error (ioError, userError)
import Text.Show (Show, show)

----------------Version---------------
data Version = Version Int Int Int
	deriving (Eq)

instance Show Version where
	show (Version a b c) =
		"Version: "
		++ (show a)
		++ "."
		++ (show b)
		++ "."
		++ (show c)

tlVersion :: Version
tlVersion = Version 0 0 1

tlHelp :: String
tlHelp = "Interpreter for the TL Language"

----------------------Options----------------------
data Action = ParseOnly | Interpret 
	deriving (Show, Eq)

data Options = Options
	{
		optHelp :: Bool,
		optVersion :: Bool,
		optAction :: Action
	}
	deriving (Show)

defaultOptions :: Options
defaultOptions = Options
	{
		optHelp = False,
		optVersion = False,
		optAction = Interpret
	}

options :: [OptDescr (Options -> Options)]
options = 	[
	Option ['v'] ["version"] (NoArg (\opts -> opts {optVersion = True})) "Show version of Tl Interpreter",
	Option ['h'] ["help"] (NoArg (\opts -> opts {optHelp = True})) "Show help for Tl Interpreter",
	Option ['p'] ["parse"] (NoArg (\opts -> opts {optAction = ParseOnly})) "Parse TL program",
	Option ['i'] ["interpret"] (NoArg (\opts -> opts {optAction = Interpret})) "Interpret TL program"
	]

tlOptions :: [String] -> IO (Options, String)
tlOptions argv = case getOpt Permute options argv of
	(o, [n],[] ) -> return  $ (foldl (flip id) defaultOptions o, n)
	(_, _  ,[] ) -> ioError $ userError $ exactlyone ++ usageInfo header options
	(_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo header options
	where
		header = "Usage: TL [OPTION...] filename"
		exactlyone = "There must be exactly one TL input file"
