module TL.StdLib.Stdlib (
    functions,
    M.lookup
    ) where

import           Data.Function (($))
import           Data.List ((++))
import qualified Data.Map as M
import           Data.String (String)
import           Prelude (error)
import           System.IO (IO)
import qualified TL.StdLib.StdIO as StdIO
import qualified TL.StdLib.StdIntMap as StdM
import qualified TL.StdLib.StdList as StdL
import           TL.Token (Token(..), Expression)
import           Text.Show (show)

functions :: M.Map String (Expression -> IO(Token))
functions = M.fromList [
    ("sum", totalTL),
    ("product", productTL),
    ("length", lengthTL),
    ("member", memberTL),
    ("insert", insertTL),
    ("union", unionTL),
    ("sort", StdL.sortTL),
    ("flatten", StdL.concatTL),
    ("enumFromTo", StdL.enumFromToTL),
    ("intmap", StdM.intMapTL),
    ("lookup", StdM.lookupTL),
    ("readfile", StdIO.readFileTL),
    ("writefile", StdIO.writeFileTL),
    ("appendfile", StdIO.appendFileTL),
    ("getchar", StdIO.getCharTL)
    ]

lengthTL :: Expression -> IO(Token)
lengthTL [LiteralL lst] = StdL.lengthTL([LiteralL lst])
lengthTL [LiteralM m] = StdM.sizeTL([LiteralM m])
lengthTL param = error $ "length called with incorrect parameters: " ++ show param

totalTL :: Expression -> IO(Token)
totalTL [LiteralL lst] = StdL.totalTL([LiteralL lst])
totalTL [LiteralM m] = StdM.sumTL([LiteralM m])
totalTL param = error $ "total called with incorrect parameters: " ++ show param

productTL :: Expression -> IO(Token)
productTL [LiteralL lst] = StdL.productTL([LiteralL lst])
productTL [LiteralM m] = StdM.productTL([LiteralM m])
productTL param = error $ "product called with incorrect parameters: " ++ show param

memberTL :: Expression -> IO(Token)
memberTL [e, LiteralL lst] = StdL.elemTL([e, LiteralL lst])
memberTL [e, LiteralM m] = StdM.memberTL([e, LiteralM m])
memberTL param = error $ "member called with incorrect parameters: " ++ show param

insertTL :: Expression -> IO(Token)
insertTL [e, LiteralL lst] = StdL.insertTL([e, LiteralL lst])
insertTL [LiteralI key, val, LiteralM m] = StdM.insertTL([LiteralI key, val, LiteralM m])
insertTL e = error $ "insert called with incorrect parameters: " ++ show e

unionTL :: Expression -> IO(Token)
unionTL [LiteralL lst1, LiteralL lst2] = StdL.unionTL([LiteralL lst1, LiteralL lst2])
unionTL [LiteralM m1, LiteralM m2] = StdM.unionTL([LiteralM m1, LiteralM m2])
unionTL param = error $ "union called with incorrect parameters: " ++ show param
