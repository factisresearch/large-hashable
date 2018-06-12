-- | This module defines the central type class `LargeHashable` of this package.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.LargeHashable.Class (

    LargeHashable(..), largeHash, LargeHashable'(..), genericUpdateHash

) where

-- keep imports in alphabetic order (in Emacs, use "M-x sort-lines")
import Data.Bits
import Data.Fixed
import Data.Foldable
import Data.Int
import Data.LargeHashable.Intern
import Data.Proxy
import Data.Ratio
import Data.Time
import Data.Time.Clock.TAI
import Data.Void (Void)
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GHC.Generics
import GHC.TypeLits
import qualified Codec.Binary.UTF8.Light as Utf8
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Short as BS
import qualified Data.Foldable as F
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Scientific as Sci
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Strict.Tuple as Tuple
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Data.Text.Internal.Lazy as TLI
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

-- | A type class for computing hashes (i.e. MD5, SHA256, ...) from
-- haskell values.
--
-- The laws of this typeclass are the following:
--
-- (1) If two values are equal
-- according to '==', then the finally computed hashes must also be equal
-- according to '=='. However it is not required that the hashes of inequal
-- values have to be inequal. Also note that an instance of 'LargeHashable'
-- does not require a instance of 'Eq'. Using any sane algorithm the chance
-- of a collision should be 1 / n where n is the number of different hashes
-- possible.
--
-- (2) If two values are inequal
-- according to '==', then the probability of a hash collision is 1/n,
-- where n is the number of possible hashes produced by the
-- underlying hash algorithm.
--
-- A rule of thumb: hash all information that you would also need for
-- serializing/deserializing values of your datatype. For instance, when
-- hashing lists, you would not only hash the list elements but also the
-- length of the list. Consider the following datatype
--
-- > data Foo = Foo [Int] [Int]
--
-- We now write an instance for LargeHashable like this
--
-- > instance LargeHashable Foo where
-- >     updateHash (Foo l1 l2) = updateHash l1 >> updateHash l2
--
-- If we did not hash the length of a list, then the following two values
-- of @Foo@ would produce identical hashes:
--
-- > Foo [1,2,3] []
-- > Foo [1] [2,3]
--
class LargeHashable a where
    updateHash :: a -> LH ()
    default updateHash :: (GenericLargeHashable (Rep a), Generic a) => a -> LH ()
    updateHash = genericUpdateHash

class LargeHashable' t where
    updateHash' :: LargeHashable a => t a -> LH ()

-- | 'largeHash' is the central function of this package.
--   For a given value it computes a 'Hash' using the given
--   'HashAlgorithm'.
largeHash :: LargeHashable a => HashAlgorithm h -> a -> h
largeHash algo x = runLH algo (updateHash x)

{-# INLINE updateHashTextData #-}
updateHashTextData :: T.Text -> LH ()
updateHashTextData !t = do
    updates <- hashUpdates
    ioInLH $ do
        TF.useAsPtr t $ \valPtr units ->
            hu_updatePtr updates (castPtr valPtr) (fromIntegral (2 * units))
        return ()

{-# INLINE updateHashText #-}
updateHashText :: T.Text -> LH ()
updateHashText !t = do
    updateHashTextData t
    updates <- hashUpdates
    ioInLH $ hu_updateULong updates (fromIntegral (T.length t))

instance LargeHashable T.Text where
    updateHash = updateHashText

{-# INLINE updateHashLazyText #-}
updateHashLazyText :: Int -> TL.Text -> LH ()
updateHashLazyText !len (TLI.Chunk !t !next) = do
    updateHashTextData t
    updateHashLazyText (len + T.length t) next
updateHashLazyText !len TLI.Empty = updateHash len

instance LargeHashable TL.Text where
    updateHash = updateHashLazyText 0

{-# INLINE updateHashByteStringData #-}
updateHashByteStringData :: B.ByteString -> LH ()
updateHashByteStringData !b = do
    updates <- hashUpdates
    ioInLH $ do
        ptr <- B.useAsCString b return
        hu_updatePtr updates (castPtr ptr) (B.length b)

{-# INLINE updateHashByteString #-}
updateHashByteString :: B.ByteString -> LH ()
updateHashByteString !b = do
    updateHashByteStringData b
    updates <- hashUpdates
    ioInLH $ hu_updateULong updates (fromIntegral (B.length b))

instance LargeHashable B.ByteString where
    updateHash = updateHashByteString

{-# INLINE updateHashLazyByteString #-}
updateHashLazyByteString :: Int -> BL.ByteString -> LH ()
updateHashLazyByteString !len (BLI.Chunk !bs !next) = do
    updateHashByteStringData bs
    updateHashLazyByteString (len + B.length bs) next
updateHashLazyByteString !len BLI.Empty = updateHash len

instance LargeHashable BL.ByteString where
    updateHash = updateHashLazyByteString 0

instance LargeHashable BS.ShortByteString where
    updateHash = updateHash . BS.fromShort

{-# INLINE updateHashWithFun #-}
updateHashWithFun :: (HashUpdates -> a -> IO ()) -> a -> LH ()
updateHashWithFun f x =
    do updates <- hashUpdates
       ioInLH $ f updates x

instance LargeHashable Int where
    updateHash = updateHashWithFun hu_updateLong . fromIntegral

instance LargeHashable Int8 where
    updateHash = updateHashWithFun hu_updateChar

instance LargeHashable Int16 where
    updateHash = updateHashWithFun hu_updateShort

instance LargeHashable Int32 where
    updateHash = updateHashWithFun hu_updateInt

instance LargeHashable Int64 where
    updateHash = updateHashWithFun hu_updateLong

instance LargeHashable Word where
    updateHash = updateHashWithFun hu_updateLong . fromIntegral

instance LargeHashable Word8 where
    updateHash = updateHashWithFun hu_updateUChar

instance LargeHashable Word16 where
    updateHash = updateHashWithFun hu_updateUShort

instance LargeHashable Word32 where
    updateHash = updateHashWithFun hu_updateUInt

instance LargeHashable Word64 where
    updateHash = updateHashWithFun hu_updateULong . fromIntegral

instance LargeHashable CChar where
    updateHash (CChar i) = updateHashWithFun hu_updateChar i

instance LargeHashable CShort where
    updateHash (CShort i) = updateHashWithFun hu_updateShort i

instance LargeHashable CInt where
    updateHash (CInt i) = updateHashWithFun hu_updateInt i

instance LargeHashable CLong where
    updateHash (CLong i) = updateHashWithFun hu_updateLong (fromIntegral i)

instance LargeHashable CUChar where
    updateHash (CUChar w) = updateHashWithFun hu_updateUChar w

instance LargeHashable CUShort where
    updateHash (CUShort w) = updateHashWithFun hu_updateUShort w

instance LargeHashable CUInt where
    updateHash (CUInt w) = updateHashWithFun hu_updateUInt w

instance LargeHashable CULong where
    updateHash (CULong w) = updateHashWithFun hu_updateULong (fromIntegral w)

instance LargeHashable Char where
    updateHash = updateHashWithFun hu_updateUInt . Utf8.c2w

{-# INLINE updateHashInteger #-}
updateHashInteger :: Integer -> LH ()
updateHashInteger !i
    | i == 0 = updateHash (0 :: CUChar)
    | i > 0  = do
        updateHash (fromIntegral (i .&. 0xffffffffffffffff) :: CULong)
        updateHashInteger (shift i (-64))
    | otherwise = do
        updateHash (0 :: CUChar) -- prepend 0 to show it is negative
        updateHashInteger (abs i)

instance LargeHashable Integer where
    updateHash = updateHashInteger

foreign import ccall doubleToWord64 :: Double -> Word64

instance LargeHashable Double where
    updateHash = updateHash . doubleToWord64

foreign import ccall floatToWord32 :: Float -> Word32

instance LargeHashable Float where
    updateHash = updateHash . floatToWord32

{-# INLINE updateHashFixed #-}
updateHashFixed :: HasResolution a => Fixed a -> LH ()
updateHashFixed f = updateHash (truncate . (* f) . fromInteger $ resolution f :: Integer)

instance HasResolution a => LargeHashable (Fixed a) where
    updateHash = updateHashFixed

{-# INLINE updateHashBool #-}
updateHashBool :: Bool -> LH ()
updateHashBool True  = updateHash (1 :: CUChar)
updateHashBool False = updateHash (0 :: CUChar)

instance LargeHashable Bool where
    updateHash = updateHashBool

{-# INLINE updateHashList #-}
updateHashList :: LargeHashable a => [a] -> LH ()
updateHashList = loop 0
    where
      loop :: LargeHashable a => Int -> [a] -> LH ()
      loop !i [] =
          updateHash i
      loop !i (x:xs) = do
          updateHash x
          loop (i + 1) xs

instance LargeHashable a => LargeHashable [a] where
    updateHash = updateHashList

{-# INLINE setFoldFun #-}
setFoldFun :: LargeHashable a => LH () -> a -> LH ()
setFoldFun action value = action >> updateHash value

{-# INLINE updateHashSet #-}
updateHashSet :: LargeHashable a => S.Set a -> LH ()
updateHashSet !set = do
    foldl' setFoldFun (return ()) set -- Note: foldl' for sets traverses the elements in asc order
    updateHash (S.size set)

instance LargeHashable a => LargeHashable (S.Set a) where
    updateHash = updateHashSet

{-# INLINE updateHashIntSet #-}
updateHashIntSet :: IntSet.IntSet -> LH ()
updateHashIntSet !set = do
    IntSet.foldl' setFoldFun (return ()) set
    updateHash (IntSet.size set)

-- Lazy and Strict IntSet share the same definition
instance LargeHashable IntSet.IntSet where
    updateHash = updateHashIntSet

{-# INLINE updateHashHashSet #-}
updateHashHashSet :: LargeHashable a => HashSet.HashSet a -> LH ()
updateHashHashSet !set =
    -- The ordering of elements in a set does not matter. A HashSet does not
    -- offer an efficient way of exctracting its elements in some specific
    -- ordering. So we use the auxiliary function 'hashListModuloOrdering'.
    hashListModuloOrdering (HashSet.size set) (HashSet.toList set)

-- | Hashes a list of values such the two permutations of the same list
-- yields the same hash.
hashListModuloOrdering :: LargeHashable a => Int -> [a] -> LH ()
hashListModuloOrdering len list =
    do updateXorHash (map updateHash list)
       updateHash len

-- Lazy and Strict HashSet share the same definition
instance LargeHashable a => LargeHashable (HashSet.HashSet a) where
    updateHash = updateHashHashSet

{-# INLINE mapFoldFun #-}
mapFoldFun :: (LargeHashable k, LargeHashable a) => LH () -> k -> a -> LH ()
mapFoldFun action key value = action >> updateHash key >> updateHash value

{-# INLINE updateHashMap #-}
updateHashMap :: (LargeHashable k, LargeHashable a) => M.Map k a -> LH ()
updateHashMap !m = do
        M.foldlWithKey' mapFoldFun (return ()) m
        updateHash (M.size m)

-- Lazy and Strict Map share the same definition
instance (LargeHashable k, LargeHashable a) => LargeHashable (M.Map k a) where
    updateHash = updateHashMap

{-# INLINE updateHashIntMap #-}
updateHashIntMap :: LargeHashable a => IntMap.IntMap a -> LH ()
updateHashIntMap !m = do
    IntMap.foldlWithKey' mapFoldFun (return ()) m
    updateHash (IntMap.size m)

-- Lazy and Strict IntMap share the same definition
instance LargeHashable a => LargeHashable (IntMap.IntMap a) where
    updateHash = updateHashIntMap

updateHashHashMap :: (LargeHashable k, LargeHashable v) => HashMap.HashMap k v -> LH ()
updateHashHashMap !m =
    -- The ordering of elements in a map do not matter. A HashMap does not
    -- offer an efficient way of exctracting its elements in some specific
    -- ordering. So we use the auxiliary function 'hashListModuloOrdering'.
    hashListModuloOrdering (HashMap.size m) (HashMap.toList m)

-- Lazy and Strict HashMap share the same definition
instance (LargeHashable k, LargeHashable v) => LargeHashable (HashMap.HashMap k v) where
    updateHash = updateHashHashMap

{-# INLINE updateHashTuple #-}
updateHashTuple :: (LargeHashable a, LargeHashable b) => (a, b) -> LH ()
updateHashTuple (!a, !b) = updateHash a >> updateHash b

instance (LargeHashable a, LargeHashable b) => LargeHashable (a, b) where
    updateHash = updateHashTuple

{-# INLINE updateHashTriple #-}
updateHashTriple :: (LargeHashable a, LargeHashable b, LargeHashable c) => (a, b, c) -> LH ()
updateHashTriple (a, b, c) = updateHash a >> updateHash b >> updateHash c

instance (LargeHashable a, LargeHashable b, LargeHashable c) => LargeHashable (a, b, c) where
    updateHash = updateHashTriple

{-# INLINE updateHashQuadruple #-}
updateHashQuadruple :: (LargeHashable a, LargeHashable b, LargeHashable c, LargeHashable d) => (a, b, c, d) -> LH ()
updateHashQuadruple (a, b, c, d) = updateHash a >> updateHash b >> updateHash c >> updateHash d

instance (LargeHashable a, LargeHashable b, LargeHashable c, LargeHashable d) => LargeHashable (a, b, c, d) where
    updateHash = updateHashQuadruple

{-# INLINE updateHashQuintuple #-}
updateHashQuintuple :: (LargeHashable a, LargeHashable b, LargeHashable c, LargeHashable d, LargeHashable e) => (a, b, c, d, e) -> LH ()
updateHashQuintuple (a, b, c, d, e) = updateHash a >> updateHash b >> updateHash c >> updateHash d >> updateHash e

instance (LargeHashable a, LargeHashable b, LargeHashable c, LargeHashable d, LargeHashable e) => LargeHashable (a, b, c, d, e) where
    updateHash = updateHashQuintuple

updateHashMaybe :: LargeHashable a => Maybe a -> LH ()
updateHashMaybe Nothing   = updateHash (0 :: CULong)
updateHashMaybe (Just !x) = updateHash (1 :: CULong) >> updateHash x

instance LargeHashable a => LargeHashable (Maybe a) where
    updateHash = updateHashMaybe

instance (LargeHashable a, LargeHashable b) => LargeHashable (Either a b) where
    updateHash (Left !l)  = updateHash (0 :: CULong) >> updateHash l
    updateHash (Right !r) = updateHash (1 :: CULong) >> updateHash r

instance LargeHashable () where
    updateHash () = return ()

instance LargeHashable Ordering where
    updateHash EQ = updateHash (0  :: CULong)
    updateHash GT = updateHash (-1 :: CULong)
    updateHash LT = updateHash (1  :: CULong)

instance (Integral a, LargeHashable a) => LargeHashable (Ratio a) where
    updateHash !i = do
        updateHash $ numerator i
        updateHash $ denominator i

instance LargeHashable AbsoluteTime where
    updateHash t = updateHash $ diffAbsoluteTime t taiEpoch

instance LargeHashable DiffTime where
    -- could be replaced by diffTimeToPicoseconds as soon as
    -- time 1.6 becomes more common
    updateHash = updateHash . (fromRational . toRational :: DiffTime -> Pico)

instance LargeHashable NominalDiffTime where
    updateHash = updateHash . (fromRational . toRational :: NominalDiffTime -> Pico)

instance LargeHashable LocalTime where
    updateHash (LocalTime d tod) = updateHash d >> updateHash tod

instance LargeHashable ZonedTime where
    updateHash (ZonedTime lt tz) = updateHash lt >> updateHash tz

instance LargeHashable TimeOfDay where
    updateHash (TimeOfDay h m s) = updateHash h >> updateHash m >> updateHash s

instance LargeHashable TimeZone where
    updateHash (TimeZone mintz summerOnly name) =
        updateHash mintz >> updateHash summerOnly >> updateHash name

instance LargeHashable UTCTime where
    updateHash (UTCTime d dt) = updateHash d >> updateHash dt

instance LargeHashable Day where
    updateHash (ModifiedJulianDay d) = updateHash d

instance LargeHashable UniversalTime where
    updateHash (ModJulianDate d) = updateHash d

instance LargeHashable a => LargeHashable (V.Vector a) where
    updateHash = updateHash . V.toList

instance (LargeHashable a, LargeHashable b) => LargeHashable (Tuple.Pair a b) where
    updateHash (x Tuple.:!: y) =
        do updateHash x
           updateHash y

instance LargeHashable Sci.Scientific where
    updateHash notNormalized =
        do let n = Sci.normalize notNormalized
           updateHash (Sci.coefficient n)
           updateHash (Sci.base10Exponent n)

instance LargeHashable J.Value where
    updateHash v =
        case v of
          J.Object obj ->
              do updateHash (0::Int)
                 updateHash obj
          J.Array arr ->
              do updateHash (1::Int)
                 updateHash arr
          J.String t ->
              do updateHash (2::Int)
                 updateHash t
          J.Number n ->
              do updateHash (3::Int)
                 updateHash n
          J.Bool b ->
              do updateHash (4::Int)
                 updateHash b
          J.Null ->
              updateHash (5::Int)

instance LargeHashable Void where
    updateHash _ = error "I'm void"

instance LargeHashable a => LargeHashable (Seq.Seq a) where
    updateHash = updateHash . F.toList

genericUpdateHash :: (Generic a, GenericLargeHashable (Rep a)) => a -> LH ()
genericUpdateHash = updateHashGeneric . from

-- | Support for generically deriving 'LargeHashable' instances.
-- Any instance of the type class 'GHC.Generics.Generic' can be made
-- an instance of 'LargeHashable' by an empty instance declaration.
class GenericLargeHashable f where
    updateHashGeneric :: f p -> LH ()

instance GenericLargeHashable V1 where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric = undefined

instance GenericLargeHashable U1 where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric U1 = updateHash ()

instance (GenericLargeHashable f, GenericLargeHashable g) => GenericLargeHashable (f :*: g) where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric (x :*: y) = updateHashGeneric x >> updateHashGeneric y

instance GenericLargeHashableSum (f :+: g) 0 => GenericLargeHashable (f :+: g) where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric x = updateHashGenericSum x (Proxy :: Proxy 0)

instance LargeHashable c => GenericLargeHashable (K1 i c) where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric x = updateHash (unK1 x)

-- ignore meta-info (for now)
instance (GenericLargeHashable f) => GenericLargeHashable (M1 i t f) where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric x = updateHashGeneric (unM1 x)

class GenericLargeHashableSum (f :: * -> *) (n :: Nat) where
    updateHashGenericSum :: f p -> Proxy n -> LH ()


instance (KnownNat n, GenericLargeHashable f, GenericLargeHashableSum g (n+1))
    => GenericLargeHashableSum (f :+: g) n where
    {-# INLINE updateHashGenericSum #-}
    updateHashGenericSum (L1 x) p = do
        updateHash (fromInteger (natVal p) :: CULong)
        updateHashGeneric x
    updateHashGenericSum (R1 x) _ = updateHashGenericSum x (Proxy :: Proxy (n+1))

instance (KnownNat n, GenericLargeHashable f)
    => GenericLargeHashableSum (M1 i t f) n where
    {-# INLINE updateHashGenericSum #-}
    updateHashGenericSum x p = do
        updateHash (fromInteger (natVal p) :: CULong)
        updateHashGeneric (unM1 x)
