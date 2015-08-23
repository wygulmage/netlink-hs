module Scripts.Helpers
(mkIncludeBlock, getDefinitions, getEnums, selectDefines, selectEnum, mkEnum,
mkFlag)
where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Char (isNumber, toLower, toUpper)
import Data.Function (on)
import Data.List (isPrefixOf, isInfixOf, notElem, sortBy)
import Data.Map (Map, elems, filterWithKey, fromList,
                 keys, mapKeys, toList)
import Data.Maybe (mapMaybe)
import Language.C.Analysis (runTrav_)
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.SemRep (GlobalDecls(..), TagDef(EnumDef),
                                   EnumType(..), Enumerator(..))
import Language.C.Data.Ident (Ident(..))
import Language.C.Data.InputStream (inputStreamFromString)
import Language.C.Data.Position (Position(..), position)
import Language.C.Parser (parseC)
import Language.C.Pretty (pretty)
import Language.C.Syntax.Constants (getCInteger)
import Language.C.Syntax.AST (CExpression(..), CExpr(..), CConstant(CIntConst), CBinaryOp(..))
import System.Process (readProcess)
import Text.Regex.PCRE ((=~))

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
mkEnum name vals = (name : map fst values,
                    ty : "" : join (map makeConst values))
  where
    ty = "newtype " ++ name ++ " = " ++
          name ++
          " Int deriving (Eq, Enum, Integral, Num, Ord, Real, Show)"
    makeConst (n, v) = [n ++ " :: (Num a) => a",
                        n ++ " = " ++ show v]
    values = sortBy (compare `on` snd) . toList . mapKeys ('e' :) $ vals

selectDefines :: String -> Map String Integer -> Map String Integer
selectDefines regex = filterWithKey (\k v -> k =~ regex)

selectEnum :: String -> [Map String Integer] -> Map String Integer
selectEnum regex = head $ filter (all (=~ regex) . keys)

full :: String -> String
full regex = "^" ++ regex ++ "$"

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
    getEnum (EnumDef (EnumType _ es _ _)) = Just $ map getEnumValue es
    getEnum _                             = Nothing
    getEnumValue (Enumerator (Ident s _ _) v _ _) = (s, evalCExpr v)
    cleanEnums = filterWithKey (\k v -> not ("_" `isPrefixOf` k)) . fromList

evalCExpr :: CExpr -> Integer
evalCExpr (CConst (CIntConst v _)) = getCInteger v
evalCExpr (CBinary CAddOp a b _)   = evalCExpr a + evalCExpr b
evalCExpr (CBinary CShlOp a b _)   = evalCExpr a * (2 ^ evalCExpr b)
evalCExpr other                    = error $ "Other: " ++ show (pretty other)


sanitize :: [[String]] -> [[String]]
sanitize (["@define",n]:x:[y]:xs) = ["@define",n,y]:sanitize xs
sanitize (["@define",x,y]:xs) = ["@define",x,y]:sanitize xs
sanitize (_:xs) = sanitize xs
sanitize [] = []

getDefinitions :: String -> IO (Map String Integer)
getDefinitions headers = do
    defines <- map words . lines <$> readDefines headers
    let isDefine (c:n:_) = c == "#define" && '(' `notElem` n && head n /= '_' 
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
    return $ fromList (map clean realDefines)
  where readDefines = readProcess "gcc" ["-E", "-dM", "-"]
        preprocess  = readProcess "gcc" ["-E", "-"]
