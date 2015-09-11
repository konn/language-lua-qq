{-# LANGUAGE ExplicitNamespaces, OverloadedStrings, PatternGuards #-}
{-# LANGUAGE TemplateHaskell                                      #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Lua.QQ (mkExpQQ, lua, mkStatQQ, lstat, mkBlockQQ, lblk) where
import Language.Lua.Lift

import           Data.Data                  (Data)
import           Data.Generics              (mkQ)
import           Data.List                  (intercalate)
import           Language.Haskell.Meta      (parseExp)
import           Language.Haskell.Meta      (parsePat)
import           Language.Haskell.TH        (ExpQ)
import           Language.Haskell.TH        (PatQ)
import           Language.Haskell.TH        (location)
import           Language.Haskell.TH.Quote  (QuasiQuoter (..))
import           Language.Haskell.TH.Quote  (dataToExpQ)
import           Language.Haskell.TH.Quote  (dataToPatQ)
import           Language.Haskell.TH.Syntax (Loc (..))
import           Language.Lua
import qualified Language.Lua               as L
import qualified Text.Parsec.LTok           as L

mkExpQQ :: String -> QuasiQuoter
mkExpQQ hs = QuasiQuoter { quoteExp = qExp hs L.exp
                       , quotePat = qPat hs L.exp
                       , quoteType = error "No type quote for lua!"
                       , quoteDec  = error "No dec quote for lua!"
                       }

lua :: QuasiQuoter
lua = mkExpQQ "hs"

mkStatQQ :: String -> QuasiQuoter
mkStatQQ hs = QuasiQuoter { quoteExp = qExp hs L.stat
                        , quotePat = qPat hs L.stat
                        , quoteType = error "No type quote for lua!"
                        , quoteDec  = error "No dec quote for lua!"
                        }

lstat :: QuasiQuoter
lstat = mkStatQQ "hs"

mkBlockQQ :: String -> QuasiQuoter
mkBlockQQ hs = QuasiQuoter { quoteExp = qExp hs L.chunk
                           , quotePat = qPat hs L.chunk
                           , quoteType = error "No type quote for lua!"
                           , quoteDec  = error "No dec quote for lua!"
                           }

lblk :: QuasiQuoter
lblk = mkBlockQQ "hs"

qExp :: Data a => String -> L.Parser a -> String -> ExpQ
qExp hs p src = do
  Loc f _ _ (l,c) _ <- location
  case parseNamedText p (intercalate ":" [f, show l, show c]) src of
    Right x -> antiExp hs x
    Left er -> fail $ show er

antiExp :: Data a => String -> a -> ExpQ
antiExp hs = dataToExpQ (mkQ Nothing trans)
  where
    trans (PrefixExp (PEFunCall (NormalFunCall (PEVar (VarName n)) (Args [String src0]))))
      | n == hs =
        let src = show $ L.pprint src0
        in case parseExp src of
          Left err -> Just $ fail err
          Right e  -> Just $ return e
    trans (PrefixExp (PEFunCall (MethodCall (PEVar (VarName n)) typ (Args [String src0]))))
      | n == hs =
        let src = show $ L.pprint src0
        in case parseExp src of
          Left err -> Just $ fail err
          Right e  ->
            case typ of
              "raw"  -> Just $ return e
              "lift" -> Just $ [| liftExp $(return e) |]
              "enum" -> Just $ [| liftExp (fromEnum $(return e)) |]
              _   -> Just $ fail $ "Antiquoter not supported in expression context: " ++ typ
    trans _ = Nothing

qPat :: Data a => String -> L.Parser a -> String -> PatQ
qPat hs p src = case parseText p src of
  Right x -> antiPat hs x
  Left er -> fail $ show er

antiPat :: Data a => String -> a -> PatQ
antiPat hs = dataToPatQ (mkQ Nothing trans)
  where
    trans (PEFunCall (MethodCall (PEVar (VarName n)) typ (Args [String src])))
      | n == hs =
        case parsePat src of
          Left err -> Just $ fail err
          Right e  ->
            case typ of
              "p" -> Just $ return e
              _   -> Just $ fail $ "Antiquoter not supported in pattern context: " ++ typ
    trans _ = Nothing
