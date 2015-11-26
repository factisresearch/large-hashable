-- | Generic, low-level data types for hashing. This is an internal module.
--
-- You should only import this module if you write your own hash algorithm
-- or if you need access to low-level hashing functions when defining
-- instances of 'LargeHash'.
--
-- Regular users should not import this module. Import 'Data.LargeHashable'
-- instead.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.LargeHashable.Intern (

    HashUpdates(..), HashAlgorithm(..), LH, Hash(..)
  , hashUpdates, ioInLH, runLH

) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as Base16

newtype Hash = Hash { unHash :: BS.ByteString }
    deriving (Eq)

instance Show Hash where
    show (Hash bs) =
        BSC.unpack (Base16.encode bs)

-- | Functions for updating an intermediate hash value. The functions live
-- in the 'IO' monad because they are typically implemented via FFI.
-- FIXME: functions for primitive types other than CULong missing.
data HashUpdates
    = HashUpdates
    { hu_updatePtr :: !(Ptr Word8 -> Int -> IO ()) -- ^ adds a byte array to the hash
    , hu_updateULong :: !(CULong -> IO ())         -- ^ adds an `CULong` value to the hash
    }

-- | The interface for a hashing algorithm. The interface contains a simple run
-- function, which is used to update the hash with all values needed, and the
-- outputs the resulting hash.
data HashAlgorithm
    = HashAlgorithm
    { ha_run :: (HashUpdates -> IO ()) -> IO Hash
    }

-- | The 'LH' monad (`LH` stands for "large hash") is used in the definition of
-- hashing functions for arbitrary data types.
newtype LH a = LH { _unLH :: ReaderT HashUpdates IO a }
    deriving (Functor, Applicative, Monad)

hashUpdates :: LH HashUpdates
hashUpdates = LH ask

-- | Perform an 'IO' action in the 'LH' monad. Use with care, do not perform
-- arbitrary 'IO' operation with this function! Only use it for calling
-- functions of the 'HashUpdates' datatype.
ioInLH :: IO a -> LH a
ioInLH = LH . liftIO

-- | Runs a 'LH' computation and returns the resulting hash.
{-# NOINLINE runLH #-}
runLH :: HashAlgorithm -> LH () -> Hash
runLH alg (LH lh) =
    unsafePerformIO $
    ha_run alg $
    runReaderT lh
