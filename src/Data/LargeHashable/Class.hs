-- | This module defines the central type class `LargeHashable` of this package.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Data.LargeHashable.Class (

    LargeHashable(..), largeHash, largeHashStable, LargeHashable'(..), genericUpdateHash,
    updateHashList
) where

-- keep imports in alphabetic order (in Emacs, use "M-x sort-lines")
import Data.Bits
import Data.Char (ord)
import Data.Fixed
import Data.Foldable
import Data.Int
import Data.LargeHashable.Endianness
import Data.LargeHashable.Intern
import Data.Ratio
import Data.Time
import Data.Time.Clock.TAI
import Data.Void (Void)
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GHC.Generics
import qualified Data.Aeson as J
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
#endif
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
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Foreign as TF
import qualified Data.Text.Internal.Lazy as TLI
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Data.Kind (Type)

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
    updateHashStable :: a -> LH ()
    default updateHashStable :: (GenericLargeHashable (Rep a), Generic a) => a -> LH ()
    updateHashStable = genericUpdateHashStable

class LargeHashable' t where
    updateHash' :: LargeHashable a => t a -> LH ()
    updateHashStable' :: LargeHashable a => t a -> LH ()

-- | 'largeHash' is the central function of this package.
--   For a given value it computes a 'Hash' using the given
--   'HashAlgorithm'. The library tries to keep the
--   hash values for @LargeHashable@ instances provided by
--   library stable across releases, but there is no guarantee.
--   See @largeHashStable&
largeHash :: LargeHashable a => HashAlgorithm h -> a -> h
largeHash algo x = runLH algo (updateHash x)

-- | 'largeHashStable' is similar to @largeHash@, but the hash
--   value is guaranteed to remain stable across releases,
--   even if this causes performance to degrade.
largeHashStable :: LargeHashable a => HashAlgorithm h -> a -> h
largeHashStable algo x = runLH algo (updateHashStable x)

{-# INLINE updateHashTextData #-}
updateHashTextData :: T.Text -> LH ()
updateHashTextData !t = do
    updates <- hashUpdates
    ioInLH $ do
#if MIN_VERSION_text(2,0,0)
        TF.useAsPtr t $ \(valPtr :: Ptr Word8) (units :: TF.I8) ->
            hu_updatePtr updates (castPtr valPtr) (fromIntegral units)
#else
        -- UTF-16 encoding
        TF.useAsPtr t $ \(valPtr :: Ptr Word16) (units :: TF.I16) ->
            hu_updatePtr updates (castPtr valPtr) (fromIntegral (2 * units))
#endif
        return ()

{-# INLINE updateHashText #-}
updateHashText :: T.Text -> LH ()
updateHashText !t = do
    updateHashTextData t
    updates <- hashUpdates
    ioInLH $ hu_updateULong updates (fromIntegral (T.length t))

{-# INLINE updateHashStableTextData #-}
updateHashStableTextData :: T.Text -> LH ()
updateHashStableTextData t = do
    let bs =
            case systemEndianness of
                LittleEndian -> TE.encodeUtf16LE t
                BigEndian -> TE.encodeUtf16BE t
    updateHashByteStringData bs

{-# INLINE updateHashStableText #-}
updateHashStableText :: T.Text -> LH ()
updateHashStableText t = do
    updateHashStableTextData t
    updates <- hashUpdates
    ioInLH $ hu_updateULong updates (fromIntegral (T.length t))

instance LargeHashable T.Text where
    updateHash = updateHashText
    updateHashStable = updateHashStableText

{-# INLINE updateHashLazyText #-}
updateHashLazyText :: Int -> TL.Text -> LH ()
updateHashLazyText !len (TLI.Chunk !t !next) = do
    updateHashTextData t
    updateHashLazyText (len + T.length t) next
updateHashLazyText !len TLI.Empty = updateHash len

{-# INLINE updateHashStableLazyText #-}
updateHashStableLazyText :: Int -> TL.Text -> LH ()
updateHashStableLazyText !len (TLI.Chunk !t !next) = do
    updateHashStableTextData t
    updateHashStableLazyText (len + T.length t) next
updateHashStableLazyText !len TLI.Empty = updateHash len

instance LargeHashable TL.Text where
    updateHash = updateHashLazyText 0
    updateHashStable = updateHashStableLazyText 0

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
    updateHashStable = updateHash

{-# INLINE updateHashLazyByteString #-}
updateHashLazyByteString :: Int -> BL.ByteString -> LH ()
updateHashLazyByteString !len (BLI.Chunk !bs !next) = do
    updateHashByteStringData bs
    updateHashLazyByteString (len + B.length bs) next
updateHashLazyByteString !len BLI.Empty = updateHash len

instance LargeHashable BL.ByteString where
    updateHash = updateHashLazyByteString 0
    updateHashStable = updateHash

instance LargeHashable BS.ShortByteString where
    updateHash = updateHash . BS.fromShort
    updateHashStable = updateHash

{-# INLINE updateHashWithFun #-}
updateHashWithFun :: (HashUpdates -> a -> IO ()) -> a -> LH ()
updateHashWithFun f x =
    do updates <- hashUpdates
       ioInLH $ f updates x

instance LargeHashable Int where
    updateHash = updateHashWithFun hu_updateULong . fromIntegral
    updateHashStable = updateHash

instance LargeHashable Int8 where
    updateHash = updateHashWithFun hu_updateUChar . fromIntegral
    updateHashStable = updateHash

instance LargeHashable Int16 where
    updateHash = updateHashWithFun hu_updateUShort . fromIntegral
    updateHashStable = updateHash

instance LargeHashable Int32 where
    updateHash = updateHashWithFun hu_updateUInt . fromIntegral
    updateHashStable = updateHash

instance LargeHashable Int64 where
    updateHash = updateHashWithFun hu_updateULong . fromIntegral
    updateHashStable = updateHash

instance LargeHashable Word where
    updateHash = updateHashWithFun hu_updateULong . fromIntegral
    updateHashStable = updateHash

instance LargeHashable Word8 where
    updateHash = updateHashWithFun hu_updateUChar
    updateHashStable = updateHash

instance LargeHashable Word16 where
    updateHash = updateHashWithFun hu_updateUShort
    updateHashStable = updateHash

instance LargeHashable Word32 where
    updateHash = updateHashWithFun hu_updateUInt
    updateHashStable = updateHash

instance LargeHashable Word64 where
    updateHash = updateHashWithFun hu_updateULong . fromIntegral
    updateHashStable = updateHash

instance LargeHashable CChar where
    updateHash (CChar i) = updateHashWithFun hu_updateUChar (fromIntegral i)
    updateHashStable = updateHash

instance LargeHashable CShort where
    updateHash (CShort i) = updateHashWithFun hu_updateUShort (fromIntegral i)
    updateHashStable = updateHash

instance LargeHashable CInt where
    updateHash (CInt i) = updateHashWithFun hu_updateUInt (fromIntegral i)
    updateHashStable = updateHash

instance LargeHashable CLong where
    updateHash (CLong i) = updateHashWithFun hu_updateULong (fromIntegral i)
    updateHashStable = updateHash

instance LargeHashable CUChar where
    updateHash (CUChar w) = updateHashWithFun hu_updateUChar w
    updateHashStable = updateHash

instance LargeHashable CUShort where
    updateHash (CUShort w) = updateHashWithFun hu_updateUShort w
    updateHashStable = updateHash

instance LargeHashable CUInt where
    updateHash (CUInt w) = updateHashWithFun hu_updateUInt w
    updateHashStable = updateHash

instance LargeHashable CULong where
    updateHash (CULong w) = updateHashWithFun hu_updateULong (fromIntegral w)
    updateHashStable = updateHash

instance LargeHashable Char where
    updateHash = updateHashWithFun hu_updateUInt . c2w
    updateHashStable = updateHash

c2w :: Char -> Word32
{-# INLINE c2w #-}
c2w c = toEnum (ord c :: Int)

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
    updateHashStable = updateHash

foreign import ccall doubleToWord64 :: Double -> Word64

instance LargeHashable Double where
    updateHash = updateHash . doubleToWord64
    updateHashStable = updateHash

foreign import ccall floatToWord32 :: Float -> Word32

instance LargeHashable Float where
    updateHash = updateHash . floatToWord32
    updateHashStable = updateHash

{-# INLINE updateHashFixed #-}
updateHashFixed :: HasResolution a => Fixed a -> LH ()
updateHashFixed f = updateHash (truncate . (* f) . fromInteger $ resolution f :: Integer)

instance HasResolution a => LargeHashable (Fixed a) where
    updateHash = updateHashFixed
    updateHashStable = updateHash

{-# INLINE updateHashBool #-}
updateHashBool :: Bool -> LH ()
updateHashBool True  = updateHash (1 :: CUChar)
updateHashBool False = updateHash (0 :: CUChar)

instance LargeHashable Bool where
    updateHash = updateHashBool
    updateHashStable = updateHash

{-# INLINE updateHashList #-}
updateHashList :: forall a. (a -> LH ()) -> [a] -> LH ()
updateHashList f = loop 0
    where
      loop :: Int -> [a] -> LH ()
      loop !i [] =
          updateHash i
      loop !i (x:xs) = do
          f x
          loop (i + 1) xs

instance LargeHashable a => LargeHashable [a] where
    updateHash = updateHashList updateHash
    updateHashStable = updateHashList updateHashStable

{-# INLINE setFoldFun #-}
setFoldFun :: LargeHashable a => (a -> LH ()) -> LH () -> a -> LH ()
setFoldFun f action value = action >> f value

{-# INLINE updateHashSet #-}
updateHashSet :: LargeHashable a => (a -> LH ()) -> S.Set a -> LH ()
updateHashSet !f !set = do
    foldl' (setFoldFun f) (return ()) set -- Note: foldl' for sets traverses the elements in asc order
    updateHash (S.size set)

instance LargeHashable a => LargeHashable (S.Set a) where
    updateHash = updateHashSet updateHash
    updateHashStable = updateHashSet updateHashStable

{-# INLINE updateHashIntSet #-}
updateHashIntSet :: IntSet.IntSet -> LH ()
updateHashIntSet !set = do
    IntSet.foldl' (setFoldFun updateHash) (return ()) set
    updateHash (IntSet.size set)

-- Lazy and Strict IntSet share the same definition
instance LargeHashable IntSet.IntSet where
    updateHash = updateHashIntSet
    updateHashStable = updateHash

{-# INLINE updateHashHashSet #-}
updateHashHashSet :: LargeHashable a => (a -> LH ()) -> HashSet.HashSet a -> LH ()
updateHashHashSet !f !set =
    -- The ordering of elements in a set does not matter. A HashSet does not
    -- offer an efficient way of exctracting its elements in some specific
    -- ordering. So we use the auxiliary function 'hashListModuloOrdering'.
    hashListModuloOrdering f (HashSet.size set) (HashSet.toList set)

-- | Hashes a list of values such the two permutations of the same list
-- yields the same hash.
hashListModuloOrdering :: LargeHashable a => (a -> LH ()) -> Int -> [a] -> LH ()
hashListModuloOrdering f len list =
    do updateXorHash (map f list)
       updateHash len

-- Lazy and Strict HashSet share the same definition
instance LargeHashable a => LargeHashable (HashSet.HashSet a) where
    updateHash = updateHashHashSet updateHash
    updateHashStable = updateHashHashSet updateHashStable

{-# INLINE mapFoldFun #-}
mapFoldFun :: (LargeHashable k, LargeHashable a) =>
    (k -> LH ()) -> (a -> LH ()) -> LH () -> k -> a -> LH ()
mapFoldFun kf vf action key value = action >> kf key >> vf value

{-# INLINE updateHashMap #-}
updateHashMap :: (LargeHashable k, LargeHashable a) =>
    (k -> LH ()) -> (a -> LH ()) -> M.Map k a -> LH ()
updateHashMap !kf !vf !m = do
        M.foldlWithKey' (mapFoldFun kf vf) (return ()) m
        updateHash (M.size m)

-- Lazy and Strict Map share the same definition
instance (LargeHashable k, LargeHashable a) => LargeHashable (M.Map k a) where
    updateHash = updateHashMap updateHash updateHash
    updateHashStable = updateHashMap updateHashStable updateHashStable

{-# INLINE updateHashIntMap #-}
updateHashIntMap :: LargeHashable a => (a -> LH ()) -> IntMap.IntMap a -> LH ()
updateHashIntMap !f !m = do
    IntMap.foldlWithKey' (mapFoldFun updateHash f) (return ()) m
    updateHash (IntMap.size m)

-- Lazy and Strict IntMap share the same definition
instance LargeHashable a => LargeHashable (IntMap.IntMap a) where
    updateHash = updateHashIntMap updateHash
    updateHashStable = updateHashIntMap updateHashStable

updateHashHashMap :: (LargeHashable k, LargeHashable v) =>
    ((k, v) -> LH ()) -> HashMap.HashMap k v -> LH ()
updateHashHashMap !f !m =
    -- The ordering of elements in a map do not matter. A HashMap does not
    -- offer an efficient way of exctracting its elements in some specific
    -- ordering. So we use the auxiliary function 'hashListModuloOrdering'.
    hashListModuloOrdering f (HashMap.size m) (HashMap.toList m)

-- Lazy and Strict HashMap share the same definition
instance (LargeHashable k, LargeHashable v) => LargeHashable (HashMap.HashMap k v) where
    updateHash = updateHashHashMap updateHash
    updateHashStable = updateHashHashMap updateHashStable

instance (LargeHashable a, LargeHashable b) => LargeHashable (a, b) where
    updateHash (!a, !b) = updateHash a >> updateHash b
    updateHashStable (!a, !b) = updateHashStable a >> updateHashStable b

instance (LargeHashable a, LargeHashable b, LargeHashable c) => LargeHashable (a, b, c) where
    updateHash (a, b, c) = updateHash a >> updateHash b >> updateHash c
    updateHashStable (a, b, c) = updateHashStable a >> updateHashStable b >> updateHashStable c

instance (LargeHashable a, LargeHashable b, LargeHashable c, LargeHashable d) => LargeHashable (a, b, c, d) where
    updateHash (a, b, c, d) = updateHash a >> updateHash b >> updateHash c >> updateHash d
    updateHashStable (a, b, c, d) =
        updateHashStable a >> updateHashStable b >> updateHashStable c >> updateHashStable d

instance (LargeHashable a, LargeHashable b, LargeHashable c, LargeHashable d, LargeHashable e) => LargeHashable (a, b, c, d, e) where
    updateHash (a, b, c, d, e) =
        updateHash a >> updateHash b >> updateHash c >> updateHash d >> updateHash e
    updateHashStable (a, b, c, d, e) =
        updateHashStable a >> updateHashStable b >> updateHashStable c >> updateHashStable d >> updateHashStable e

instance LargeHashable a => LargeHashable (Maybe a) where
    updateHash Nothing   = updateHash (0 :: CULong)
    updateHash (Just !x) = updateHash (1 :: CULong) >> updateHash x
    updateHashStable Nothing   = updateHash (0 :: CULong)
    updateHashStable (Just !x) = updateHash (1 :: CULong) >> updateHashStable x

instance (LargeHashable a, LargeHashable b) => LargeHashable (Either a b) where
    updateHash (Left !l)  = updateHash (0 :: CULong) >> updateHash l
    updateHash (Right !r) = updateHash (1 :: CULong) >> updateHash r
    updateHashStable (Left !l)  = updateHash (0 :: CULong) >> updateHashStable l
    updateHashStable (Right !r) = updateHash (1 :: CULong) >> updateHashStable r

instance LargeHashable () where
    updateHash () = return ()
    updateHashStable () = return ()

instance LargeHashable Ordering where
    updateHash EQ = updateHash (0  :: CULong)
    updateHash GT = updateHash (-1 :: CULong)
    updateHash LT = updateHash (1  :: CULong)
    updateHashStable = updateHash

instance (Integral a, LargeHashable a) => LargeHashable (Ratio a) where
    updateHash !i = do
        updateHash $ numerator i
        updateHash $ denominator i
    updateHashStable = updateHash

instance LargeHashable AbsoluteTime where
    updateHash t = updateHash $ diffAbsoluteTime t taiEpoch
    updateHashStable = updateHash

instance LargeHashable DiffTime where
    -- could be replaced by diffTimeToPicoseconds as soon as
    -- time 1.6 becomes more common
    updateHash = updateHash . (fromRational . toRational :: DiffTime -> Pico)
    updateHashStable = updateHash

instance LargeHashable NominalDiffTime where
    updateHash = updateHash . (fromRational . toRational :: NominalDiffTime -> Pico)
    updateHashStable = updateHash

instance LargeHashable LocalTime where
    updateHash (LocalTime d tod) = updateHash d >> updateHash tod
    updateHashStable = updateHash

instance LargeHashable ZonedTime where
    updateHash (ZonedTime lt tz) = updateHash lt >> updateHash tz
    updateHashStable = updateHash

instance LargeHashable TimeOfDay where
    updateHash (TimeOfDay h m s) = updateHash h >> updateHash m >> updateHash s
    updateHashStable = updateHash

instance LargeHashable TimeZone where
    updateHash (TimeZone mintz summerOnly name) =
        updateHash mintz >> updateHash summerOnly >> updateHash name
    updateHashStable (TimeZone mintz summerOnly name) =
        updateHashStable mintz >> updateHashStable summerOnly >> updateHashStable name

instance LargeHashable UTCTime where
    updateHash (UTCTime d dt) = updateHash d >> updateHash dt
    updateHashStable = updateHash

instance LargeHashable Day where
    updateHash (ModifiedJulianDay d) = updateHash d
    updateHashStable = updateHash

instance LargeHashable UniversalTime where
    updateHash (ModJulianDate d) = updateHash d
    updateHashStable = updateHash

instance LargeHashable a => LargeHashable (V.Vector a) where
    updateHash = updateHash . V.toList
    updateHashStable = updateHashStable . V.toList

instance (LargeHashable a, LargeHashable b) => LargeHashable (Tuple.Pair a b) where
    updateHash (x Tuple.:!: y) =
        do updateHash x
           updateHash y
    updateHashStable (x Tuple.:!: y) =
        do updateHashStable x
           updateHashStable y

instance LargeHashable Sci.Scientific where
    updateHash notNormalized =
        do let n = Sci.normalize notNormalized
           updateHash (Sci.coefficient n)
           updateHash (Sci.base10Exponent n)
    updateHashStable = updateHash

updateHashJson :: (forall a . LargeHashable a => a -> LH ()) -> J.Value -> LH ()
updateHashJson f v =
    case v of
        J.Object obj ->
            do updateHash (0::Int)
               f obj
        J.Array arr ->
            do updateHash (1::Int)
               f arr
        J.String t ->
            do updateHash (2::Int)
               f t
        J.Number n ->
            do updateHash (3::Int)
               f n
        J.Bool b ->
            do updateHash (4::Int)
               f b
        J.Null ->
            updateHash (5::Int)

instance LargeHashable J.Value where
    updateHash = updateHashJson updateHash
    updateHashStable = updateHashJson updateHashStable

#if MIN_VERSION_aeson(2,0,0)
instance LargeHashable J.Key where
    updateHash = updateHash . AesonKey.toText
    updateHashStable = updateHashStable . AesonKey.toText

instance LargeHashable v => LargeHashable (AesonKeyMap.KeyMap v) where
    updateHash v = updateHash (AesonKeyMap.toHashMap v)
    updateHashStable v = updateHashStable (AesonKeyMap.toHashMap v)
#endif

instance LargeHashable Void where
    updateHash _ = error "I'm void"

instance LargeHashable a => LargeHashable (Seq.Seq a) where
    updateHash = updateHash . F.toList
    updateHashStable = updateHashStable . F.toList

genericUpdateHash :: (Generic a, GenericLargeHashable (Rep a)) => a -> LH ()
genericUpdateHash = updateHashGeneric . from
{-# INLINE genericUpdateHash #-}

genericUpdateHashStable :: (Generic a, GenericLargeHashable (Rep a)) => a -> LH ()
genericUpdateHashStable = updateHashStableGeneric . from
{-# INLINE genericUpdateHashStable #-}

-- | Support for generically deriving 'LargeHashable' instances.
-- Any instance of the type class 'GHC.Generics.Generic' can be made
-- an instance of 'LargeHashable' by an empty instance declaration.
class GenericLargeHashable f where
    updateHashGeneric :: f p -> LH ()
    updateHashStableGeneric :: f p -> LH ()

instance GenericLargeHashable V1 where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric = undefined
    updateHashStableGeneric = undefined

instance GenericLargeHashable U1 where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric U1 = updateHash ()
    updateHashStableGeneric U1 = updateHashStable ()

instance (GenericLargeHashable f, GenericLargeHashable g) => GenericLargeHashable (f :*: g) where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric (x :*: y) = updateHashGeneric x >> updateHashGeneric y
    updateHashStableGeneric (x :*: y) = updateHashStableGeneric x >> updateHashStableGeneric y

instance (GenericLargeHashable f, GenericLargeHashableSum g) => GenericLargeHashable (f :+: g) where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric x = updateHashGenericSum x 0
    updateHashStableGeneric x = updateHashStableGenericSum x 0

instance LargeHashable c => GenericLargeHashable (K1 i c) where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric x = updateHash (unK1 x)
    updateHashStableGeneric x = updateHashStable (unK1 x)

-- ignore meta-info (for now)
instance (GenericLargeHashable f) => GenericLargeHashable (M1 i t f) where
    {-# INLINE updateHashGeneric #-}
    updateHashGeneric x = updateHashGeneric (unM1 x)
    updateHashStableGeneric x = updateHashStableGeneric (unM1 x)

class GenericLargeHashableSum (f :: Type -> Type) where
    updateHashGenericSum :: f p -> Int -> LH ()
    updateHashStableGenericSum :: f p -> Int -> LH ()

instance (GenericLargeHashable f, GenericLargeHashableSum g)
    => GenericLargeHashableSum (f :+: g) where
    {-# INLINE updateHashGenericSum #-}
    updateHashGenericSum (L1 x) !p = do
        updateHash p
        updateHashGeneric x
    updateHashGenericSum (R1 x) !p = updateHashGenericSum x (p+1)
    updateHashStableGenericSum (L1 x) !p = do
        updateHashStable p
        updateHashStableGeneric x
    updateHashStableGenericSum (R1 x) !p = updateHashStableGenericSum x (p+1)

instance (GenericLargeHashable f) => GenericLargeHashableSum (M1 i t f) where
    {-# INLINE updateHashGenericSum #-}
    updateHashGenericSum x !p = do
        updateHash p
        updateHashGeneric (unM1 x)
    updateHashStableGenericSum x !p = do
        updateHashStable p
        updateHashStableGeneric (unM1 x)
