{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables #-}

module Wire where

import           Control.Wire hiding (object)
import qualified Control.Wire as Wire
import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude hiding ((.), id)

context_
  :: forall a b m e k. (Monad m, Ord k)
  => (a -> k)            -- ^ Function to turn the signal into a context.
  -> (k -> Wire e m a b) -- ^ Base wire constructor.
  -> Wire e m a b
context_ key constructor = context_' Map.empty
  where
    context_' :: Map k (Wire e m a b) -> Wire e m a b
    context_' !contexts = mkGen $ \dt x -> do
      let context = key x
          wire0   = constructor context
          wire    = Map.findWithDefault wire0 context contexts
      (mx, wire') <- stepWire wire dt x
      return (mx, context_' $ Map.insert context wire' contexts)
