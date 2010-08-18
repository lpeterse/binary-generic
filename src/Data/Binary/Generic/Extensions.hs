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
import Data.List
import Data.Bits
import Data.ByteString.Lazy (ByteString)  
import Data.Text.Lazy (Text) 
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import Control.Monad

type Extension a = (Get a, a -> Put) -> (Get a, a -> Put)


extGet :: (Monad m, Typeable a, Typeable b) => m a -> m b -> m a
extGet  = extR

extPut :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
extPut  = extQ

defaultExtension :: (Typeable a) => Extension a
defaultExtension  =   byteStringExt 
                    . textExt 
                    . floatExt 
                    . integerExt
                    . wordExt 
                    . intExt 
                    . charExt

integerExt      :: (Typeable a) => Extension a
integerExt (g,p) = let g' = g `extGet` (getInteger :: Get Integer     )
                       p' = p `extPut` (putInteger :: Integer   -> Put)
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


-----------------------------------------------------------------------
-- fixing inconsistent byteorder of Integer to big-endian
-----------------------------------------------------------------------


{-# INLINE putInteger #-}
putInteger  :: Integer -> Put 
putInteger n | n >= lo && n <= hi = do
        putWord8 0
        put (fromIntegral n :: Int32)  -- fast path
     where
        lo = fromIntegral (minBound :: Int32) :: Integer
        hi = fromIntegral (maxBound :: Int32) :: Integer
putInteger n = do
        putWord8 1
        put sign
        put $ reverse (unroll (abs n))         -- unroll the bytes
     where
        sign = fromIntegral (signum n) :: Word8

{-# INLINE getInteger #-}
getInteger  :: Get Integer
getInteger   = do
        tag <- get :: Get Word8
        case tag of
            0 -> liftM fromIntegral (get :: Get Int32)
            _ -> do sign  <- get
                    bytes <- get
                    let v = roll (reverse bytes)
                    return $! if sign == (1 :: Word8) then v else - v

--
-- Fold and unfold an Integer to and from a list of its bytes
--
unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

