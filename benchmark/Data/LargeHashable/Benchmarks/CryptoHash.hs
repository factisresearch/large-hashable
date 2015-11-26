{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
module Data.LargeHashable.Benchmarks.CryptoHash where

import Data.LargeHashable.Intern (Hash(..))
import qualified Crypto.Hash as H
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Byteable
import Data.List (foldl')
import Data.Word
import Data.Bits

data HashAlgorithm
    = MD5
    | SHA256
    | SHA512
      deriving (Eq, Show)

data HashCtx = forall h . H.HashAlgorithm h => HashCtx !(H.Context h)

hashMd5 :: LargeHashable h => h -> Hash
hashMd5 h =
    let ctx = hashInit MD5
    in hashFinish (hashUpdate ctx h)

hashInit :: HashAlgorithm -> HashCtx
hashInit alg =
    case alg of
      MD5 -> HashCtx (H.hashInit :: H.Context H.MD5)
      SHA256 -> HashCtx (H.hashInit :: H.Context H.SHA256)
      SHA512 -> HashCtx (H.hashInit :: H.Context H.SHA512)

hashFinish :: HashCtx -> Hash
hashFinish (HashCtx x) = Hash (toBytes $ H.hashFinalize x)

updateFromBuilder :: HashCtx -> B.Builder -> HashCtx
updateFromBuilder (HashCtx ctx) builder =
    HashCtx (H.hashUpdates ctx (BSL.toChunks (B.toLazyByteString builder)))

{-# SPECIALIZE hashUpdate :: HashCtx -> T.Text -> HashCtx #-}
class LargeHashable a where
    hashUpdate :: HashCtx -> a -> HashCtx

instance LargeHashable BS.ByteString where
    hashUpdate (HashCtx x) bs = HashCtx (H.hashUpdate x bs)

instance LargeHashable Int where
    hashUpdate (HashCtx ctx) i =
        -- we can make this faster by accessing the machine represenation of an int
        let w = (fromIntegral (toInteger i)) :: Word64
        in HashCtx (H.hashUpdate ctx (BS.pack [(fromIntegral (shiftR w 56) :: Word8)
                                              ,(fromIntegral (shiftR w 48) :: Word8)
                                              ,(fromIntegral (shiftR w 40) :: Word8)
                                              ,(fromIntegral (shiftR w 32) :: Word8)
                                              ,(fromIntegral (shiftR w 24) :: Word8)
                                              ,(fromIntegral (shiftR w 16) :: Word8)
                                              ,(fromIntegral (shiftR w  8) :: Word8)
                                              ,(fromIntegral (w)           :: Word8)]))

instance LargeHashable T.Text where
    hashUpdate (HashCtx ctx) t = HashCtx (H.hashUpdate ctx (T.encodeUtf8 t))

instance LargeHashable a => LargeHashable [a] where
    hashUpdate ctx l =
        foldl' hashUpdate ctx l
