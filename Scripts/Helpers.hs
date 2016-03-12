module Helpers
  ( mkIncludeBlock
  , getDefinitions
  , getEnums
  , selectDefines
  , selectEnum
  , mkEnum
  , mkFlag
  , selectEnums
  )
where

--import Control.Applicative ((<$>))

import Control.Monad (join)
import Data.Char (isNumber)
import Data.Function (on)
import Data.List (isPrefixOf, sortBy, nubBy)
import Data.Map (Map, elems, filterWithKey, fromList, insert,
                 keys, mapKeys, toList, lookup, empty)
import Data.Maybe (mapMaybe, fromJust)
import Language.C.Analysis (runTrav_)
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.SemRep (GlobalDecls(..), TagDef(EnumDef),
                                   EnumType(..), Enumerator(..))
import Language.C.Data.Ident (Ident(..))
import Language.C.Data.InputStream (inputStreamFromString)
import Language.C.Data.Position (position)
import Language.C.Parser (parseC)
import Language.C.Pretty (pretty)
import Language.C.Syntax.Constants (getCInteger)
import Language.C.Syntax.AST (CExpression(..), CExpr, CConstant(CIntConst), CBinaryOp(..))
import System.Process (readProcess)
import Text.Regex.PCRE ((=~))

import Prelude hiding (lookup)

mkIncludeBlock :: [String] -> String
mkIncludeBlock = unlines . map (\e -> "#include <" ++ e ++ ">")

mkFlag :: String -> Map String Integer -> ([String], [String])
mkFlag name vals = (name : map fst values,
                    ty : "" : join (map makeConst values))
  where
    ty = "newtype " ++ name ++ " = " ++
          name ++
          " Int deriving (Bits, Eq, Enum, Integral, Num, Ord, Real, Show)"
    makeConst (n, v) = [n ++ " :: (Num a, Bits a) => a",
                        n ++ " = " ++ show v]
    values = sortBy (compare `on` snd) . toList . mapKeys ("f" ++) $ vals

mkEnum :: String -> Map String Integer -> ([String], [String])
mkEnum name vals = (name : fName : map fst values,
                    ty : "" : fun : "" : join (map makeConst values))
  where
    ty = "newtype " ++ name ++ " = " ++
          name ++
          " Int deriving (Eq, Enum, Integral, Num, Ord, Real, Show)"
    makeConst (n, v) = [n ++ " :: (Num a) => a",
                        n ++ " = " ++ show v]
    values = sortBy (compare `on` snd) . toList . mapKeys ('e' :) $ vals
    (fName, fun) = showEnum name vals

showEnum :: String -> Map String Integer -> (String, String)
showEnum name vals = (fName,
  fName ++ " :: (Num a) => (Show a) => (Eq a) => a -> String\n" ++
  concatMap makeLine values ++
  fName ++ " i = \"" ++ name ++ " #\" ++ (show i)\n")
  where
    makeLine (n, v) = fName ++ ' ':(show v) ++ " = \"" ++ n ++ "\"\n"
    values = nubBy ((==) `on` snd) . sortBy (compare `on` snd) . toList $vals
    fName = "show" ++ name

selectDefines :: String -> Map String Integer -> Map String Integer
selectDefines regex = filterWithKey (\k _ -> k =~ regex)

selectEnum :: String -> [Map String Integer] -> Map String Integer
selectEnum regex = head . selectEnums regex

selectEnums :: String -> [Map String Integer] -> [Map String Integer]
selectEnums regex = filter (all (=~ regex) . keys)

--full :: String -> String
--full regex = "^" ++ regex ++ "$"

getEnums :: String -> IO [Map String Integer]
getEnums source = do
    parsed <- flip parseC initPos . inputStreamFromString <$> preprocessed
    let unit = gTags . fst . check $ runTrav_ (analyseAST $ check parsed)
        enums = mapMaybe getEnum (elems unit)
    return $ map cleanEnums enums
  where
    check (Left err) = error $ show err
    check (Right a)   = a
    preprocessed = readProcess "gcc" ["-E", "-"] source
    initPos = position 0 "" 0 0
    cleanEnums = filterWithKey (\k _ -> not ("_" `isPrefixOf` k))


getEnum :: TagDef -> Maybe (Map String Integer)
getEnum (EnumDef (EnumType _ es _ _)) = Just $foldl getEnumValue empty es
getEnum _ = Nothing

getEnumValue :: Map String Integer -> Enumerator -> Map String Integer
getEnumValue m (Enumerator (Ident s _ _) v _ _) = insert s a m
  where a = evalEExpr m v

evalEExpr :: Map String Integer -> CExpr -> Integer
evalEExpr _ (CConst (CIntConst v _)) = getCInteger v
evalEExpr m (CBinary CAddOp a b _)   = evalEExpr m a + evalEExpr m b
evalEExpr m (CBinary CSubOp a b _)   = evalEExpr m a - evalEExpr m b
evalEExpr m (CBinary CShlOp a b _)   = evalEExpr m a * (2 ^ evalEExpr m b)
evalEExpr m (CVar (Ident a _ _) _)   = fromJust $lookup a m
evalEExpr _ other                    = error $ "Other: " ++ show (pretty other)


--evalCExpr :: CExpr -> Integer
--evalCExpr = evalEExpr empty


sanitize :: [[String]] -> [[String]]
sanitize (["@define",n]:_:[y]:xs) = ["@define",n,y]:sanitize xs
sanitize (["@define",x,y]:xs) = ["@define",x,y]:sanitize xs
sanitize (_:xs) = sanitize xs
sanitize [] = []

getDefinitions :: String -> IO (Map String Integer)
getDefinitions headers = do
    defines <- map words . lines <$> readDefines headers
    let isDefine (c:n:_) = c == "#define" && '(' `notElem` n && head n /= '_' 
        isDefine xs = error ("isDefine: Couldn't check: " ++ show xs)
        hasValue = (>= 3) . length
        names = map (!! 1) $ filter (\d -> isDefine d && hasValue d) defines
        kludge = map (\n -> "@define \"" ++ n ++ "\" " ++ n) names
    defines2 <- map words . lines <$> preprocess (headers ++ unlines kludge)
    let isInteresting d = hasValue d &&
                           head d == "@define" &&
                           (all isNumber (d !! 2) ||
                            "0x" `isPrefixOf` (d !! 2) &&
                            all isNumber (drop 2 (d !! 2)))
        realDefines = map (take 2 . drop 1) $ filter isInteresting $ sanitize defines2
        clean [k,v] = (init (tail k), read v)
        clean _ = error "Clean: got a weird list"
    return $ fromList (map clean realDefines)
  where readDefines = readProcess "gcc" ["-E", "-dM", "-"]
        preprocess  = readProcess "gcc" ["-E", "-"]
