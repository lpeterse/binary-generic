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
-- The following primitive datatypes are used as basecases and get serialized
-- according to their instances of @Data.Binary@:
-- 
-- - 'Char'
-- 
-- - 'Int'
-- 
-- - 'Word'
-- 
-- - 'Integer'
-- 
-- - 'Int8'
-- 
-- - 'Int16'
-- 
-- - 'Int32'
-- 
-- - 'Int64'
-- 
-- - 'Word8'
-- 
-- - 'Word16'
-- 
-- - 'Word32'
-- 
-- - 'Word64'
-- 
-- - 'Data.ByteString.Lazy.ByteString'
-- 
-- - 'Data.Text.Lazy.Text' encoded as Utf8
-- 
-- 'Float' and 'Double' are serialized according to 'IEEE754'.
-- For any algebraic datatype just make it an instance of class @Data.Data@
-- by simply deriving 'Data' on definition or try stand-alone-deriving.
--
-----------------------------------------------------------------------------

module Data.Binary.Generic (
    
     getGeneric
   , putGeneric

   ) where

import Data.Binary
import Data.Binary.IEEE754 (putFloat32be, getFloat32be, putFloat64be, getFloat64be)
import Data.ByteString.Lazy (ByteString)  

import Data.Data
import Data.Generics

import Data.Word ()
import Data.Int  
import Data.Text.Lazy (Text) 
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)


getGeneric :: (Data a) => Get a
getGeneric  = generalCase 
              `extR` (get          :: Get Char)
              `extR` (get          :: Get Int)
              `extR` (get          :: Get Word)
              `extR` (get          :: Get Integer)
              `extR` (get          :: Get Int8)
              `extR` (get          :: Get Int16)
              `extR` (get          :: Get Int32)
              `extR` (get          :: Get Int64)
              `extR` (get          :: Get Word8)
              `extR` (get          :: Get Word16)
              `extR` (get          :: Get Word32)
              `extR` (get          :: Get Word64)
              `extR` (get          :: Get ByteString)
              `extR` (getText      :: Get Text)
              `extR` (getFloat32be :: Get Float)
              `extR` (getFloat64be :: Get Double)
              where
                getText       = get >>= (return . decodeUtf8)
                fromIntegralM = return . fromIntegral
                myDataType    = dataTypeOf ((undefined :: Get b -> b) generalCase)
                generalCase   = let imax  = maxConstrIndex myDataType
                                    index | imax == 1   = return 1 :: Get Int
                                          | imax <= 255 = (get :: Get Word8)  >>= fromIntegralM
                                          | otherwise   = (get :: Get Word16) >>= fromIntegralM
                                in  index >>= \i-> fromConstrM getGeneric (indexConstr myDataType i)

putGeneric  :: (Data a) => a -> Put 
putGeneric   = generalCase 
               `extQ` (put              :: Char       -> Put)
               `extQ` (put              :: Int        -> Put) 
               `extQ` (put              :: Word       -> Put) 
               `extQ` (put              :: Integer    -> Put)
               `extQ` (put              :: Int8       -> Put)
               `extQ` (put              :: Int16      -> Put)
               `extQ` (put              :: Int32      -> Put)
               `extQ` (put              :: Int64      -> Put)
               `extQ` (put              :: Word8      -> Put)
               `extQ` (put              :: Word16     -> Put)
               `extQ` (put              :: Word32     -> Put)
               `extQ` (put              :: Word64     -> Put)
               `extQ` (put              :: ByteString -> Put)
               `extQ` (put . encodeUtf8 :: Text       -> Put)
               `extQ` (putFloat32be     :: Float      -> Put)
               `extQ` (putFloat64be     :: Double     -> Put)
               where
                 generalCase t = let i    = fromIntegral $ constrIndex (toConstr t)
                                     imax = maxConstrIndex (dataTypeOf t) 
                                     putIndex | imax == 1   =                      return ()
                                              | imax <= 255 = put (i :: Word8)  >> return ()
                                              | otherwise   = put (i :: Word16) >> return ()
                                 in  foldl (>>) putIndex (gmapQ putGeneric t) 
  

