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
-- You can build your own type-specific stacks. For example the default stack
-- looks like this, whereas the ordering determines, which function matches
-- first for a specific type. This especially allows you to override the
-- default choices:
--
-- > getExtDefault    :: Typeable a => Get a -> Get a
-- > getExtDefault     = getExtInteger
-- >                   . getExtChar
-- >                   . getExtWord
-- >                   . getExtInt
-- >                   . getExtFloat
-- >                   . getExtText
-- >                   . getExtByteString
-- >
-- > putExtDefault    :: Typeable a => (a -> Put) -> a -> Put
-- > putExtDefault     = putExtInteger
-- >                   . putExtChar
-- >                   . putExtWord
-- >                   . putExtInt
-- >                   . putExtFloat
-- >                   . putExtText
-- >                   . putExtByteString
-- 
-- Notice that these stacks have to be grounded, ideally with something
-- that handles algebraic types.
-- Have a look at @Data.Binary.Generic@ how this is done for the default 
-- stack.
-- 
-- IMPORTANT: You cannot simply apply an extension to 'getGeneric' or
-- 'putGeneric', since these do a recursive call at the bottom level
-- which points to the top of the stack.
-- 
--
-----------------------------------------------------------------------------

module Data.Binary.Generic.Extensions (
    
     extGet
   , extPut

   , getExtDefault
   , putExtDefault

   , getExtInteger
   , putExtInteger
   , getExtChar
   , putExtChar
   , getExtWord
   , putExtWord
   , getExtInt
   , putExtInt
   , getExtFloat
   , putExtFloat
   , getExtText
   , putExtText
   , getExtByteString
   , putExtByteString

   ) where

import Data.Binary
import Data.Binary.IEEE754 (putFloat32be, getFloat32be, putFloat64be, getFloat64be)

import Data.Data
import Data.Generics

import Data.Word ()
import Data.Int  
import Data.List
import Data.Bits
import Data.ByteString.Lazy              (ByteString)  

import qualified Data.Text               as T 
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as LT 
import qualified Data.Text.Lazy.Encoding as LTE

import Control.Monad


extGet           :: (Monad m, Typeable a, Typeable b) => m b -> m a -> m a
extGet            = flip extR

extPut           :: (Typeable a, Typeable b) => (b -> q) -> (a -> q) -> a -> q
extPut            = flip extQ


getExtDefault    :: Typeable a => Get a -> Get a
getExtDefault     = getExtInteger
                  . getExtChar
                  . getExtWord
                  . getExtInt
                  . getExtFloat
                  . getExtText
                  . getExtByteString

putExtDefault    :: Typeable a => (a -> Put) -> a -> Put
putExtDefault     = putExtInteger
                  . putExtChar
                  . putExtWord
                  . putExtInt
                  . putExtFloat
                  . putExtText
                  . putExtByteString


getExtInteger    :: Typeable a => Get a -> Get a
getExtInteger     = extGet         (getInteger :: Get        Integer) 

putExtInteger    :: Typeable a => (a -> Put) -> a -> Put 
putExtInteger     = extPut         (putInteger :: Integer     -> Put)


getExtChar       :: Typeable a => Get a -> Get a
getExtChar        = extGet         (get        :: Get           Char) 

putExtChar       :: Typeable a => (a -> Put) -> a -> Put 
putExtChar        = extPut         (put        :: Char        -> Put)


getExtWord       :: Typeable a => Get a -> Get a
getExtWord        =  extGet        (get        :: Get         Word  )
                  .  extGet        (get        :: Get         Word8 ) 
                  .  extGet        (get        :: Get         Word16) 
                  .  extGet        (get        :: Get         Word32) 
                  .  extGet        (get        :: Get         Word64) 

putExtWord       :: Typeable a => (a -> Put) -> a -> Put 
putExtWord        =  extPut        (put        :: Word        -> Put) 
                  .  extPut        (put        :: Word8       -> Put) 
                  .  extPut        (put        :: Word16      -> Put) 
                  .  extPut        (put        :: Word32      -> Put) 
                  .  extPut        (put        :: Word64      -> Put)  
 

getExtInt        :: Typeable a => Get a -> Get a
getExtInt         =  extGet        (get        :: Get         Int  )
                  .  extGet        (get        :: Get         Int8 ) 
                  .  extGet        (get        :: Get         Int16) 
                  .  extGet        (get        :: Get         Int32) 
                  .  extGet        (get        :: Get         Int64) 

putExtInt        :: Typeable a => (a -> Put) -> a -> Put 
putExtInt         =  extPut        (put        :: Int         -> Put) 
                  .  extPut        (put        :: Int8        -> Put) 
                  .  extPut        (put        :: Int16       -> Put) 
                  .  extPut        (put        :: Int32       -> Put) 
                  .  extPut        (put        :: Int64       -> Put)  


getExtFloat      :: Typeable a => Get a -> Get a
getExtFloat       = extGet       (getFloat32be :: Get         Float)
                  . extGet       (getFloat64be :: Get        Double)

putExtFloat      :: Typeable a => (a -> Put) -> a -> Put 
putExtFloat       = extPut       (putFloat32be :: Float      -> Put)
                  . extPut       (putFloat64be :: Double     -> Put)


getExtText       :: Typeable a => Get a -> Get a
getExtText        = extGet         (getText    :: Get         T.Text) 
                  . extGet         (getTextL   :: Get        LT.Text)

putExtText       :: Typeable a => (a -> Put) -> a -> Put 
putExtText        = extPut         (putText    :: T.Text      -> Put)
                  . extPut         (putTextL   :: LT.Text     -> Put)


getExtByteString :: Typeable a => Get a -> Get a
getExtByteString  = extGet         (get        :: Get     ByteString) 

putExtByteString :: Typeable a => (a -> Put) -> a -> Put 
putExtByteString  = extPut         (put        :: ByteString  -> Put)



-------------------------------------------------------------------
-- serialisation for Data.Text
-------------------------------------------------------------------

getText  :: Get  T.Text
getText   = get >>= (return .  TE.decodeUtf8)

getTextL :: Get LT.Text
getTextL  = get >>= (return . LTE.decodeUtf8)

putText  :: T.Text  -> Put
putText   = put .  TE.encodeUtf8

putTextL :: LT.Text -> Put
putTextL  = put . LTE.encodeUtf8


-------------------------------------------------------------------
-- integer serialisation with consistent big-endian byteorder
-------------------------------------------------------------------


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

