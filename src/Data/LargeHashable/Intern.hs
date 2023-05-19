-- | Generic, low-level data types for hashing. This is an internal module.
--
-- You should only import this module if you write your own hash algorithm
-- or if you need access to low-level hashing functions when defining
-- instances of 'LargeHash'.
--
-- Regular users should not import this module. Import 'Data.LargeHashable'
-- instead.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.LargeHashable.Intern (

    HashUpdates(..), HashAlgorithm(..), LH
  , hashUpdates, ioInLH, runLH, updateXorHash

) where

-- keep imports in alphabetic order (in Emacs, use "M-x sort-lines")
import Control.Monad
import Data.Word
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

-- | Functions for updating an intermediate hash value. The functions live
-- in the 'IO' monad because they are typically implemented via FFI.
data HashUpdates
    = HashUpdates
    { hu_updatePtr :: {-# NOUNPACK #-} !(Ptr Word8 -> Int -> IO ()) -- ^ adds a byte array to the hash
    , hu_updateUChar :: {-# NOUNPACK #-} !(Word8 -> IO ())    -- Word8
    , hu_updateUShort :: {-# NOUNPACK #-} !(Word16 -> IO ())  -- Word16
    , hu_updateUInt :: {-# NOUNPACK #-} !(Word32 -> IO ())      -- Word32
    , hu_updateULong :: {-# NOUNPACK #-} !(Word64 -> IO ())    -- Word64
    }

-- | The interface for a hashing algorithm. The interface contains a simple run
-- function, which is used to update the hash with all values needed, and the
-- outputs the resulting hash.
data HashAlgorithm h
    = HashAlgorithm
    { ha_run :: {-# NOUNPACK #-} !((HashUpdates -> IO ()) -> IO h)
    , ha_xor :: {-# NOUNPACK #-} !(h -> h -> h)
    , ha_updateHash :: {-# NOUNPACK #-} !(HashUpdates -> h -> IO ())
    }

data LHEnv
    = LHEnv
    { lh_updates :: {-# NOUNPACK #-} !HashUpdates
    , lh_updateXorHash :: {-# NOUNPACK #-} !([LH ()] -> IO ())
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
    (*>) = lhBind'

instance Monad LH where
    (>>=) = lhBind

{-# INLINE hashUpdates #-}
hashUpdates :: LH HashUpdates
hashUpdates =
    LH $ \env -> return (lh_updates env)

{-# INLINE getUpdateXorHash #-}
getUpdateXorHash :: LH ([LH ()] -> IO ())
getUpdateXorHash =
    LH $ \env -> return (lh_updateXorHash env)

-- | Perform an 'IO' action in the 'LH' monad. Use with care, do not perform
-- arbitrary 'IO' operation with this function! Only use it for calling
-- functions of the 'HashUpdates' datatype.
{-# INLINE ioInLH #-}
ioInLH :: IO a -> LH a
ioInLH io =
    LH $ \_env -> io

-- | Runs a 'LH' computation and returns the resulting hash.
{-# NOINLINE runLH #-}
runLH :: HashAlgorithm h -> LH () -> h
runLH alg lh =
    unsafePerformIO (runLH' alg lh)

runLH' :: HashAlgorithm h -> LH () -> IO h
runLH' alg (LH lh) =
    ha_run alg fun
    where
      fun updates =
          lh (LHEnv updates (updateXor updates))
      updateXor updates actions =
          do mh <- foldM foldFun Nothing actions
             case mh of
               Just h -> ha_updateHash alg updates h
               Nothing -> return ()
      foldFun mh action =
          do h2 <- runLH' alg action
             case mh of
               Nothing -> return (Just h2)
               Just h1 ->
                   let !h = ha_xor alg h1 h2
                   in return (Just h)

updateXorHash :: [LH ()] -> LH ()
updateXorHash actions =
    do f <- getUpdateXorHash
       ioInLH (f actions)
