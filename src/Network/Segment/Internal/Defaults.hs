{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Network.Segment.Internal.Defaults (gdef) where
import GHC.Generics

class GPointed' f where
  gdef' :: f x

instance GPointed' U1 where
  gdef' = U1

instance GPointed' (K1 i (Maybe a)) where
  gdef' = K1 Nothing

instance GPointed' f => GPointed' (M1 i c f) where
  gdef' = M1 gdef'

instance (GPointed' f, GPointed' g) => GPointed' (f :*: g) where
  gdef' = gdef' :*: gdef'

gdef :: (Generic a, GPointed' (Rep a)) => a
gdef = to gdef'