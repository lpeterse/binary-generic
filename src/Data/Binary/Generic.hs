{-# OPTIONS -XNoMonomorphismRestriction -XRankNTypes  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Generic
-- Copyright   : Lars Petersen
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lars Petersen <info@lars-petersen.net>
-- Stability   : experimental
--
-- 
-- For any algebraic datatype just make it an instance of class @Data.Data@
-- by simply deriving 'Data' on definition or try stand-alone-deriving.
--
-----------------------------------------------------------------------------

module Data.Binary.Generic (

     getAlgebraic
   , putAlgebraic

   , getGeneric
   , putGeneric

   , getGenericByCallback
   , putGenericByCallback

   ) where

import Data.Binary
import Data.Binary.Put     (putWord16be)
import Data.Binary.Get     (getWord16be)
import Data.Binary.Generic.Extensions

import Data.Data


getAlgebraic :: (Data a) => Get a
getAlgebraic  = getGenericByCallback getAlgebraic

putAlgebraic :: (Data a) => a -> Put
putAlgebraic  = putGenericByCallback putAlgebraic


getGeneric   :: (Data a) => Get a
getGeneric    = fst defaultStack

putGeneric   :: (Data a) => a -> Put
putGeneric    = snd defaultStack


defaultStack :: forall a. Data a => (Get a, a -> Put)
defaultStack  = defaultExtension base
              where
                g = fst defaultStack
                p = snd defaultStack
                base :: (Get a, a -> Put)
                base  = (getGenericByCallback g, putGenericByCallback p)

--------------------------------------------------------------
-- algebraic basecases with callbacks
--------------------------------------------------------------

getGenericByCallback  :: (Data a) => (forall d. Data d => Get d) -> Get a
getGenericByCallback c = generalCase 
       where
         myDataType    = dataTypeOf ((undefined :: Get b -> b) generalCase)
         typeName      = showsTypeRep (typeOf $ (undefined :: Get b -> b) generalCase) ""
         generalCase   = let imax  = maxConstrIndex myDataType
                             index | imax == 0     = error "getGeneric: constructor count is 0."
                                   | imax == 1     = return 0     :: Get Int
                                   | imax <= 256   = getWord8    >>= (return . fromIntegral)
                                   | imax <= 65536 = getWord16be >>= (return . fromIntegral)
                                   | otherwise     = error "getGeneric: constructor count out of range."
                         in  if isAlgType myDataType
                               then index >>= \i-> fromConstrM c (indexConstr myDataType (i+1))
                               else error $ "getGeneric: `" ++ typeName ++ "' is not algebraic."

putGenericByCallback    :: (Data a) => (forall d. Data d => d -> Put) -> a -> Put 
putGenericByCallback c t = let i        = fromIntegral $ constrIndex (toConstr t) - 1 
                               imax     = maxConstrIndex (dataTypeOf t) 
                               typeName = showsTypeRep (typeOf t) ""
                               putIndex | imax == 0     = error "putGeneric: constructor count is 0."
                                        | imax == 1     = return      ()
                                        | imax <= 256   = putWord8     i 
                                        | imax <= 65536 = putWord16be  i 
                                        | otherwise     = error "putGeneric: constructor count out of range."
                               in  if isAlgType (dataTypeOf t)
                                     then foldl (>>) putIndex (gmapQ c t) 
                                     else error $ "putGeneric: `" ++ typeName ++ "' is not algebraic."

