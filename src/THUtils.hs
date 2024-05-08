{-# LANGUAGE TemplateHaskell #-}
module THUtils (mkOptional) where

import Data.Generics
import Language.Haskell.TH

addMaybesAndOpts :: Maybe String -> Dec -> Q Dec
addMaybesAndOpts modName dec =
  -- Apply @rename@ and @addMaybe@ everywhere in the
  -- declaration @dec@.
  --
  -- The SYB, @everywhere (mkT (f :: a -> a)) (x :: b)@
  -- applies @f@ to all data of type @a@ in @x@, and
  -- @everywhereM (mkM (f :: a -> m a) (x :: b)@ is
  -- similar, but applies @f@ everywhere in @x@ monadically.
  everywhere (mkT rename) <$>
  everywhereM (mkM addMaybe) dec
  where
    -- Add the "Opt" suffix to a name, if it's from
    -- the given module.
    rename :: Name -> Name
    rename n = if nameModule n == modName
        then mkName $ nameBase n ++ "Opt"
        else n

    -- Wrap the type of a record field in @Maybe@.
    addMaybe :: (Name, Strict, Type) -> Q (Name, Strict, Type)
    addMaybe (n, s, ty) = do
      ty' <- [t| Maybe $(return ty) |]
      return (n,s,ty')

mkOptional :: Name -> Q Dec
mkOptional n = do
    TyConI dec <- reify n
    addMaybesAndOpts (nameModule n) dec
