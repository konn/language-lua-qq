{-# LANGUAGE DefaultSignatures, DeriveGeneric, FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TypeSynonymInstances #-}
module Language.Lua.Lift (LiftExp(..)) where

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT
import           GHC.Generics
import           Language.Lua

class LiftExp a where
  liftExp :: a -> Exp

  default liftExp :: (GLiftExp (Rep a), Generic a) => a -> Exp
  liftExp a = gLiftExp (from a)

instance LiftExp Exp where
  liftExp = id

instance LiftExp Bool where
  liftExp = Bool

instance LiftExp Int where
  liftExp = Number . show

instance LiftExp Double where
  liftExp = Number . show

instance LiftExp Integer where
  liftExp = Number . show

instance {-# OVERLAPPING #-} LiftExp [Char] where
  liftExp = String

instance {-# OVERLAPPABLE #-} LiftExp a => LiftExp [a] where
  liftExp = TableConst . map (Field . liftExp)

instance LiftExp PrefixExp where
  liftExp = PrefixExp

instance LiftExp LT.Text where
  liftExp = String . LT.unpack

instance LiftExp T.Text where
  liftExp = String . T.unpack

instance (LiftExp a, LiftExp b) => LiftExp (a, b) where
  liftExp (a, b) = TableConst $ map Field [liftExp a, liftExp b]

instance (LiftExp a, LiftExp b, LiftExp c) => LiftExp (a, b, c) where
  liftExp (a, b, c) = TableConst $ map Field [liftExp a, liftExp b, liftExp c]

instance (LiftExp a, LiftExp b, LiftExp c, LiftExp d)
         => LiftExp (a, b, c, d) where
  liftExp (a, b, c, d) =
    TableConst $ map Field [liftExp a, liftExp b, liftExp c, liftExp d]

instance (LiftExp a, LiftExp b, LiftExp c, LiftExp d, LiftExp e)
         => LiftExp (a, b, c, d, e) where
  liftExp (a, b, c, d, e) =
    TableConst $ map Field [liftExp a, liftExp b, liftExp c, liftExp d, liftExp e]

class GLiftExp a where
  gLiftExp :: a x -> Exp

instance LiftExp a => GLiftExp (K1 r a) where
  gLiftExp (K1 a) = liftExp a

instance (GLiftExp l, GLiftExp r) => GLiftExp (l :+: r) where
  gLiftExp (L1 l) = gLiftExp l
  gLiftExp (R1 r) = gLiftExp r

instance (Selector s, GLiftExp l) => GDecodeProduct (M1 S s l) where
  gDecodeProduct s@(M1 l) =
    case selName s of
      "" -> [Field $ gLiftExp l]
      sn -> [NamedField sn $ gLiftExp l]

instance (Constructor c, GDecodeProduct f) => GLiftExp (M1 C c f) where
  gLiftExp m@(M1 f) =
    let xs = gDecodeProduct f
        cn = conName m
    in if null xs
       then TableConst [ NamedField "con" (String cn)]
       else TableConst [ NamedField "con" (String cn)
                       , NamedField "args" $ TableConst xs]
class GDecodeProduct k where
  gDecodeProduct :: k x -> [TableField]

instance (Datatype c, GLiftExp f) => GLiftExp (M1 D c f) where
  gLiftExp m@(M1 a) =
    case gLiftExp a of
      TableConst xs ->
        TableConst $ NamedField "typ" (String $ datatypeName m) : xs
      _ -> error "GLiftExp cannoooot!!!"

instance LiftExp c => GDecodeProduct (K1 i c) where
  gDecodeProduct (K1 a) = [Field $ liftExp a]

instance GDecodeProduct U1 where
  gDecodeProduct U1 = []

instance (GDecodeProduct a, GDecodeProduct b) => GDecodeProduct (a :*: b) where
  gDecodeProduct (a :*: b) = gDecodeProduct a ++ gDecodeProduct b

instance LiftExp a => LiftExp (Maybe a)
