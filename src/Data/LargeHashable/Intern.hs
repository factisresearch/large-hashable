-- | Generic, low-level data types for hashing. This is an internal module.
--
-- You should only import this module if you write your own hash algorithm
-- or if you need access to low-level hashing functions when defining
-- instances of 'LargeHash'.
--
-- Regular users should not import this module. Import 'Data.LargeHashable'
-- instead.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.LargeHashable.Intern (

    HashUpdates(..), HashAlgorithm(..), LH, Hash(..), xorHash
  , hashUpdates, ioInLH, runLH, subLargeHash

) where

-- keep imports in alphabetic order (in Emacs, use "M-x sort-lines")
import Data.Bits
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC

-- | A newtype-wrapper for hash values.
newtype Hash = Hash { unHash :: BS.ByteString }
    deriving (Eq, Ord, Typeable, Generic)

xorHash :: Hash -> Hash -> Hash
xorHash (Hash h1) (Hash h2) =
    Hash (BS.pack (BS.zipWith xor h1 h2)) -- FIXME: inefficient implementation, use typed hashes

instance Show Hash where
    show (Hash bs) =
        BSC.unpack (Base16.encode bs)

-- | Functions for updating an intermediate hash value. The functions live
-- in the 'IO' monad because they are typically implemented via FFI.
data HashUpdates
    = HashUpdates
    { hu_updatePtr :: {-# NOUNPACK #-} !(Ptr Word8 -> Int -> IO ()) -- ^ adds a byte array to the hash
    , hu_updateChar :: {-# NOUNPACK #-} !(CChar -> IO ())      -- Int8
    , hu_updateUChar :: {-# NOUNPACK #-} !(CUChar -> IO ())    -- Word8
    , hu_updateShort :: {-# NOUNPACK #-} !(CShort -> IO ())    -- Int16
    , hu_updateUShort :: {-# NOUNPACK #-} !(CUShort -> IO ())  -- Word16
    , hu_updateInt :: {-# NOUNPACK #-} !(CInt -> IO ())        -- Int32
    , hu_updateUInt :: {-# NOUNPACK #-} !(CUInt -> IO ())      -- Word32
    , hu_updateLong :: {-# NOUNPACK #-} !(CLong -> IO ())      -- Int64
    , hu_updateULong :: {-# NOUNPACK #-} !(CULong -> IO ())    -- Word64
    }

-- | The interface for a hashing algorithm. The interface contains a simple run
-- function, which is used to update the hash with all values needed, and the
-- outputs the resulting hash.
data HashAlgorithm
    = HashAlgorithm
    { ha_run :: {-# NOUNPACK #-} !((HashUpdates -> IO ()) -> IO Hash)
    }

data LHEnv
    = LHEnv
    { lh_updates :: {-# NOUNPACK #-} !HashUpdates
    , lh_algorithm :: {-# NOUNPACK #-} !HashAlgorithm
    }

-- | The 'LH' monad (`LH` stands for "large hash") is used in the definition of
-- hashing functions for arbitrary data types.
newtype LH a = LH (LHEnv -> IO a)

{-# INLINE lhFmap #-}
lhFmap :: (a -> b) -> LH a -> LH b
lhFmap f (LH x) =
    LH $ \env ->
    do y <- x env
       return (f y)

{-# INLINE lhReturn #-}
lhReturn :: a -> LH a
lhReturn x = LH $ \_env -> return x

{-# INLINE lhApp #-}
lhApp :: LH (a -> b) -> LH a -> LH b
lhApp (LH f) (LH x) =
    LH $ \env -> f env <*> x env

{-# INLINE lhBind #-}
lhBind :: LH a -> (a -> LH b) -> LH b
lhBind (LH x) f =
    LH $ \env ->
    do y <- x env
       let (LH g) = f y
       g env

{-# INLINE lhBind' #-}
lhBind' :: LH a -> LH b -> LH b
lhBind' (LH x) (LH y) =
    LH $ \env ->
    do _ <- x env
       y env

instance Functor LH where
    fmap = lhFmap

instance Applicative LH where
    pure = lhReturn
    (<*>) = lhApp

instance Monad LH where
    return = lhReturn
    (>>=) = lhBind
    (>>) = lhBind'

{-# INLINE hashUpdates #-}
hashUpdates :: LH HashUpdates
hashUpdates =
    LH $ \env -> return (lh_updates env)

{-# INLINE hashAlgorithm #-}
hashAlgorithm :: LH HashAlgorithm
hashAlgorithm =
    LH $ \env -> return (lh_algorithm env)

-- | Perform an 'IO' action in the 'LH' monad. Use with care, do not perform
-- arbitrary 'IO' operation with this function! Only use it for calling
-- functions of the 'HashUpdates' datatype.
{-# INLINE ioInLH #-}
ioInLH :: IO a -> LH a
ioInLH io =
    LH $ \_env -> io

-- | Runs a 'LH' computation and returns the resulting hash.
{-# NOINLINE runLH #-}
runLH :: HashAlgorithm -> LH () -> Hash
runLH alg lh =
    unsafePerformIO (runLH' alg lh)

runLH' :: HashAlgorithm -> LH () -> IO Hash
runLH' alg (LH lh) =
    ha_run alg fun
    where
      fun updates =
          lh (LHEnv updates alg)

subLargeHash :: LH () -> LH Hash
subLargeHash lh =
    do alg <- hashAlgorithm
       ioInLH $ runLH' alg lh
