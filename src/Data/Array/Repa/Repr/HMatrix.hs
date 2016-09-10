{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

-- | the main module, containing the main parts of the interoperation code
module Data.Array.Repa.Repr.HMatrix
  (H, RepaHMatrix(..)) where

import Foreign.Storable (Storable)

import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Data.Array.Repa hiding ((!), reshape)
import Data.Array.Repa.Eval

import Numeric.LinearAlgebra hiding (rank)
import Numeric.LinearAlgebra.Data

-- | an abstract datatype representing HMatrix arrays for Repa
data H

-- | a typeclass to allow easy conversion between HMatrix types and Repa wrappers
class (Source H sh e, Element e) => RepaHMatrix sh e where
  type HMatrix sh e :: *
  -- | convert from a Repa representation to an HMatrix representation
  r2h :: Array H sh e -> HMatrix sh e
  -- | convert from an HMatrix representation to a Repa representation
  h2r :: HMatrix sh e -> Array H sh e
  -- | a utility function for the `Target` instance; make an HMatrix representation of the given Repa shape
  makeShape :: sh -> Vector e -> HMatrix sh e

instance Element e => RepaHMatrix DIM0 e where
  type HMatrix DIM0 e = e
  {-# INLINE r2h #-}
  r2h (ArrayH0 !e) = e
  {-# INLINE h2r #-}
  h2r !e = ArrayH0 e
  {-# INLINE makeShape #-}
  makeShape _ = V.head

instance (Element e, Indexable (Vector e) e) => RepaHMatrix DIM1 e where
  type HMatrix DIM1 e = Vector e
  {-# INLINE r2h #-}
  r2h (ArrayH1 !e) = e
  {-# INLINE h2r #-}
  h2r !e = ArrayH1 e
  {-# INLINE makeShape #-}
  makeShape _ = id

instance (Element e, Indexable (Vector e) e) => RepaHMatrix DIM2 e where
  type HMatrix DIM2 e = Matrix e
  {-# INLINE r2h #-}
  r2h (ArrayH2 !e) = e
  {-# INLINE h2r #-}
  h2r !e = ArrayH2 e
  {-# INLINE makeShape #-}
  makeShape (_:._:.n) = reshape n

instance Source H DIM0 e where
  newtype Array H DIM0 e = ArrayH0 e
  {-# INLINE extent #-}
  extent _ = Z
  {-# INLINE unsafeIndex #-}
  unsafeIndex (ArrayH0 !e) _ = e
  {-# INLINE index #-}
  index (ArrayH0 !e) Z = e
  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearIndex (ArrayH0 !e) _ = e
  {-# INLINE linearIndex #-}
  linearIndex (ArrayH0 !e) 0 = e
  {-# INLINE deepSeqArray #-}
  deepSeqArray !e = id

instance (Element e, Indexable (Vector e) e) => Source H DIM1 e where
  newtype Array H DIM1 e = ArrayH1 (Vector e)
  {-# INLINE extent #-}
  extent (ArrayH1 !v) = Z :. V.length v
  {-# INLINE unsafeIndex #-}
  unsafeIndex (ArrayH1 !v) (_:.i) = V.unsafeIndex v i
  {-# INLINE index #-}
  index (ArrayH1 !v) (Z:.i) = v ! i
  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearIndex (ArrayH1 !v) = V.unsafeIndex v
  {-# INLINE linearIndex #-}
  linearIndex (ArrayH1 !v) = (v !)
  {-# INLINE deepSeqArray #-}
  deepSeqArray (ArrayH1 !v) = id

instance (Element e, Indexable (Vector e) e) => Source H DIM2 e where
  newtype Array H DIM2 e = ArrayH2 (Matrix e)
  {-# INLINE extent #-}
  extent (ArrayH2 !m) = Z :. rows m :. cols m
  {-# INLINE unsafeIndex #-}
  unsafeIndex (ArrayH2 !m) (_:.i:.j) = V.unsafeIndex (m ! i) j
  {-# INLINE index #-}
  index (ArrayH2 !m) (Z:.i:.j) = m ! i ! j
  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearIndex (ArrayH2 !m) = V.unsafeIndex (flatten m)
  {-# INLINE linearIndex #-}
  linearIndex (ArrayH2 !m) = (flatten m !)
  {-# INLINE deepSeqArray #-}
  deepSeqArray (ArrayH2 !m) = id

instance RepaHMatrix sh e => Target H sh e where
  newtype MVec H sh e = MVecH { unMVecH :: IOVector e }
  {-# INLINE newMVec #-}
  newMVec = fmap MVecH . MV.new
  {-# INLINE unsafeWriteMVec #-}
  unsafeWriteMVec = MV.unsafeWrite . unMVecH
  {-# INLINE deepSeqMVec #-}
  deepSeqMVec (MVecH !mv) = id
  {-# INLINE touchMVec #-}
  touchMVec _ = return ()
  {-# INLINE unsafeFreezeMVec #-}
  unsafeFreezeMVec sh (MVecH !mv) = h2r . makeShape sh <$> V.unsafeFreeze mv
