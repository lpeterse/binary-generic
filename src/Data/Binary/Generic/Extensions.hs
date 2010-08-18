{-# OPTIONS -XNoMonomorphismRestriction #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Generic.Extensions
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

module Data.Binary.Generic.Extensions (
    
     extGet
   , extPut

   , defaultExtension

   , charExt
   , intExt
   , wordExt
   , floatExt
   , byteStringExt
   , textExt

   ) where

import Data.Binary
import Data.Binary.IEEE754 (putFloat32be, getFloat32be, putFloat64be, getFloat64be)

import Data.Data
import Data.Generics

import Data.Word ()
import Data.Int  
import Data.ByteString.Lazy (ByteString)  
import Data.Text.Lazy (Text) 
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

type Extension a = (Get a, a -> Put) -> (Get a, a -> Put)

extGet = extR
extPut = extQ

defaultExtension :: (Typeable a) => Extension a
defaultExtension  =   byteStringExt 
                    . textExt 
                    . floatExt 
                    . wordExt 
                    . integerExt
                    . intExt 
                    . charExt

integerExt      :: (Typeable a) => Extension a
integerExt (g,p) = let g' = g `extGet` (get          :: Get Integer      )
                       p' = p `extPut` (put          :: Integer    -> Put)
                   in (g',p')

charExt      :: (Typeable a) => Extension a
charExt (g,p) = let g' = g `extGet` (get          :: Get Char         )
                    p' = p `extPut` (put          :: Char       -> Put)
                in (g',p')

wordExt      :: (Typeable a) => Extension a
wordExt (g,p) = let g' = g `extGet` (get          :: Get Word         ) 
                           `extGet` (get          :: Get Word8        )
                           `extGet` (get          :: Get Word16       )
                           `extGet` (get          :: Get Word32       )
                           `extGet` (get          :: Get Word64       )
                    p' = p `extPut` (put          :: Word       -> Put)
                           `extPut` (put          :: Word8      -> Put)
                           `extPut` (put          :: Word16     -> Put)
                           `extPut` (put          :: Word32     -> Put)
                           `extPut` (put          :: Word64     -> Put)
                in (g',p')

intExt       :: (Typeable a) => Extension a
intExt  (g,p) = let g' = g `extGet` (get          :: Get Int          )  
                           `extGet` (get          :: Get Int8         )
                           `extGet` (get          :: Get Int16        )
                           `extGet` (get          :: Get Int32        )
                           `extGet` (get          :: Get Int64        )
                    p' = p `extPut` (put          :: Int        -> Put)
                           `extPut` (put          :: Int8       -> Put)
                           `extPut` (put          :: Int16      -> Put)
                           `extPut` (put          :: Int32      -> Put)
                           `extPut` (put          :: Int64      -> Put)
                in (g',p')

byteStringExt :: (Typeable a) => Extension a
byteStringExt (g,p) = let g' = g `extGet` (get    :: Get ByteString   )
                          p' = p `extPut` (put    :: ByteString -> Put)
                      in (g',p')

textExt       :: (Typeable a) => Extension a
textExt  (g,p) = let getText = get >>= (return . decodeUtf8)
                     putText = put             . encodeUtf8
                     g'      = g `extGet` (getText :: Get Text        )
                     p'      = p `extPut` (putText :: Text      -> Put)
                 in (g',p')

floatExt      :: (Typeable a) => Extension a
floatExt (g,p) = let g' = g `extGet` (getFloat32be :: Get Float       )  
                            `extGet` (getFloat64be :: Get Double      )
                     p' = p `extPut` (putFloat32be :: Float     -> Put)
                            `extPut` (putFloat64be :: Double    -> Put)
                 in (g',p')

