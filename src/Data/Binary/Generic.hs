{-# OPTIONS -XNoMonomorphismRestriction -XRankNTypes #-}

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
-- For any algebraic datatype just make it an instance of class @Data.Data.Data@
-- by simply deriving 'Data' on definition or try stand-alone-deriving. This
-- allows the library to enumerate the value constructors and thereby
-- encoding their index. Notice that serialisation depends on a type's
-- structure. Serialisations might get unreadable if the type is altered.
-- 
-- 'getGeneric' and 'putGeneric' implement a selection of type-specific 
-- defaults and are grounded by a canonical serialisation for all algebraic
-- types that instantiate 'Data.Data.Data'.
-- Have a look at @Data.Binary.Generic.Extensions@ for details.
-- 
-- If you want to ground your own type-specific stack @myStack@ of extensions
-- write the following for the @Get@-part (the @Put@-part follows
-- analogously):
-- 
-- > getMyStack :: Data a => Get a
-- > getMyStack  = myStack (getGenericByCallback getMyStack)
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

import Data.Data
import Data.Binary
import Data.Binary.Put  (putWord16be)
import Data.Binary.Get  (getWord16be)
import Data.Binary.Generic.Extensions


getAlgebraic :: Data a => Get a
getAlgebraic  = getGenericByCallback getAlgebraic

putAlgebraic :: Data a => a -> Put
putAlgebraic  = putGenericByCallback putAlgebraic


getGeneric   :: Data a => Get a
getGeneric    = getExtDefault (getGenericByCallback getGeneric)

putGeneric   :: Data a => a -> Put
putGeneric    = putExtDefault (putGenericByCallback putGeneric)


--------------------------------------------------------------
-- algebraic basecases with callbacks
--------------------------------------------------------------

getGenericByCallback  :: Data a => (forall d. Data d => Get d) -> Get a
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

putGenericByCallback    :: Data a => (forall d. Data d => d -> Put) -> a -> Put 
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

