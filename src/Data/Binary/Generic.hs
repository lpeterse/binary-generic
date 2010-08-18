{-# OPTIONS -XNoMonomorphismRestriction #-}

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
    
     getGeneric
   , putGeneric

   , getDefault
   , putDefault

   ) where

import Data.Binary
import Data.Binary.Put     (putWord16be)
import Data.Binary.Get     (getWord16be)
import Data.Binary.Generic.Extensions

import Data.Data


getGeneric :: (Data a) => Get a
getGeneric  = generalCase 
              where
                fromIntegralM = return . fromIntegral
                myDataType    = dataTypeOf ((undefined :: Get b -> b) generalCase)
                typeName      = showsTypeRep (typeOf $ (undefined :: Get b -> b) generalCase) ""
                generalCase   = let imax  = maxConstrIndex myDataType
                                    index | imax == 0     = error "getGeneric: constructor count is 0."
                                          | imax == 1     = return 0     :: Get Int
                                          | imax <= 256   = getWord8    >>= fromIntegralM
                                          | imax <= 65536 = getWord16be >>= fromIntegralM 
                                          | otherwise     = error "getGeneric: constructor count out of range."
                                in  if isAlgType myDataType
                                      then index >>= \i-> fromConstrM getDefault (indexConstr myDataType (i+1))
                                      else error $ "getGeneric: `" ++ typeName ++ "' is not algebraic."

putGeneric  :: (Data a) => a -> Put 
putGeneric t = let i        = fromIntegral $ constrIndex (toConstr t) - 1 
                   imax     = maxConstrIndex (dataTypeOf t) 
                   typeName = showsTypeRep (typeOf t) ""
                   putIndex | imax == 0     = error "putGeneric: constructor count is 0."
                            | imax == 1     = return      ()
                            | imax <= 256   = putWord8     i 
                            | imax <= 65536 = putWord16be  i 
                            | otherwise     = error "putGeneric: constructor count out of range."
               in  if isAlgType (dataTypeOf t)
                     then foldl (>>) putIndex (gmapQ putDefault t) 
                     else error $ "putGeneric: `" ++ typeName ++ "' is not algebraic."

  
defaultExtended :: (Data a) => (Get a, a -> Put)
defaultExtended  = defaultExtension (getGeneric, putGeneric)

getDefault :: (Data a) => Get a
getDefault  = fst defaultExtended

putDefault :: (Data a) => a -> Put
putDefault  = snd defaultExtended
