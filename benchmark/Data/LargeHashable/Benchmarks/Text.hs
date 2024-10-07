{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.LargeHashable.Benchmarks.Text (
    setup,
    benchTextUtf8,
    benchTextUtf16,
    benchTextUnicode,
    benchTextString
) where

import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Foreign as TF
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import Data.LargeHashable
import Data.LargeHashable.Intern
import Data.Word
import GHC.Ptr

setup :: IO (T.Text, String)
setup = do
#if MIN_VERSION_text(2,0,0)
  text <- T.readFile "test/bigfile.txt"
  let string = T.unpack text
  return (length string `seq` (text, string))
#else
  fail "benchmark for text only available with text >= 2"
#endif

-- Hash the native UTF8 encoding
benchTextUtf8 :: T.Text -> MD5Hash
benchTextUtf8 !t = runLH md5HashAlgorithm go
  where
    go = do
      updates <- hashUpdates
      ioInLH $ do
        TF.useAsPtr t $ \(valPtr :: Ptr Word8) (units :: TF.I8) ->
            hu_updatePtr updates (castPtr valPtr) (fromIntegral units)
        hu_updateULong updates (fromIntegral (T.length t))

-- Hash the UTF16 encoding for backwards compat with text-1.*
benchTextUtf16 :: T.Text -> MD5Hash
benchTextUtf16 !t =
  let b = TE.encodeUtf16BE t -- encoding of text-1.* (at least on mac with big endian)
  in runLH md5HashAlgorithm (go b)
  where
    go b = do
      updates <- hashUpdates
      ioInLH $ do
        ptr <- BS.useAsCString b return
        hu_updatePtr updates (castPtr ptr) (BS.length b)
        hu_updateULong updates (fromIntegral (T.length t))

benchTextUnicode :: T.Text -> MD5Hash
benchTextUnicode !t = runLH md5HashAlgorithm go
  where
    go = do
      updates <- hashUpdates
      ioInLH $ do
        hashStringNoLength updates (T.unpack t)
        hu_updateULong updates (fromIntegral (T.length t))

hashStringNoLength :: HashUpdates -> String -> IO ()
hashStringNoLength updates = loop
  where
    loop :: [Char] -> IO ()
    loop [] = pure ()
    loop (c:cs) = do
       _ <- hu_updateUInt updates (c2w c)
       loop cs

benchTextString:: String -> MD5Hash
benchTextString s = runLH md5HashAlgorithm $ do
  updates <- hashUpdates
  ioInLH $ hashString updates s

hashString :: HashUpdates -> String -> IO ()
hashString updates = loop 0
  where
    loop :: Int -> [Char] -> IO ()
    loop !n [] = hu_updateULong updates (fromIntegral n)
    loop !n (c:cs) = do
       _ <- hu_updateUInt updates (c2w c)
       loop (n+1) cs

{-# INLINE c2w #-}
c2w :: Char -> Word32
c2w c = toEnum (ord c :: Int)
