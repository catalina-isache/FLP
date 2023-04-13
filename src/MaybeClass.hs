module MaybeClass where

import Prelude (Show(..), (<>), undefined) -- for show instances
import qualified Data.Maybe as Maybe
import qualified Data.Bool as Bool -- for testing

import MyPrelude
import BoolClass


class MaybeClass m where
  nothing :: m a
  just :: a -> m a
  maybe :: b -> (a -> b) -> m a -> b

instance MaybeClass Maybe.Maybe where 
  nothing = Maybe.Nothing
  just = Maybe.Just
  maybe = Maybe.maybe

fromMaybe :: (MaybeClass m) => a -> m a -> a
fromMaybe def m = maybe def id m

isNothing :: (MaybeClass m, BoolClass b) => m a -> b
isNothing m = maybe true (const false) m

isJust :: (MaybeClass m, BoolClass b) => m a -> b
isJust m = maybe false (const true) m

maybeFMap :: MaybeClass m => (a -> b) -> m a -> m b
maybeFMap f m = maybe nothing (just . f) m


maybeBind :: MaybeClass m => (a -> m b) -> m a -> m b
maybeBind f m = maybe nothing f m
newtype CMaybe a = CMaybe { getCMaybe :: forall b . b -> (a -> b) -> b }

instance MaybeClass CMaybe where
  nothing = CMaybe (\n _ -> n)
  just x = CMaybe (\_ j -> j x)
  maybe n j m = getCMaybe m n j

fromMaybeClass :: (MaybeClass m, MaybeClass n) => m a -> n a 
fromMaybeClass = maybe nothing just

instance Show a => Show (CMaybe a) where
  show cm = "C" <> show (fromMaybeClass cm :: Maybe.Maybe a)
