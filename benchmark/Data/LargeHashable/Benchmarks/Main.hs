{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.LargeHashable.Benchmarks.Main (main) where

import Control.DeepSeq
import Criterion
import Criterion.Main
import Data.SafeCopy
import Data.Serialize.Put
import GHC.Generics
import qualified Data.Bytes.Serial as S
import qualified Data.LargeHashable as LH
import qualified Data.LargeHashable.Class as LH
import qualified Data.LargeHashable.Intern as LH
import qualified Data.LargeHashable.Benchmarks.CryptoHash as CH
import qualified Data.LargeHashable.Benchmarks.Serial as Serial
import qualified Data.Text as T

data Patient
    = Patient
    { p_firstName :: !T.Text
    , p_lastName :: !T.Text
    , p_note :: !T.Text
    , p_age :: !Int
    }
    deriving (Eq, Show, NFData, Generic)

$(deriveSafeCopy 0 'base ''Patient)
$(LH.deriveLargeHashable ''Patient)

instance S.Serial Patient

instance CH.LargeHashable Patient where
    hashUpdate ctx0 p =
        let !ctx1 = CH.hashUpdate ctx0 (p_firstName p)
            !ctx2 = CH.hashUpdate ctx1 (p_lastName p)
            !ctx3 = CH.hashUpdate ctx2 (p_note p)
            !ctx4 = CH.hashUpdate ctx3 (p_age p)
        in ctx4

updateHashPatient :: Patient -> LH.LH ()
updateHashPatient p =
    {-# SCC "updateHash/LargHashable" #-}
    do {-# SCC "updateHash/firstName" #-} LH.updateHash (p_firstName p)
       {-# SCC "updateHash/lastName" #-} LH.updateHash (p_lastName p)
       {-# SCC "updateHash/note" #-} LH.updateHash (p_note p)
       {-# SCC "updateHash/age" #-} LH.updateHash (p_age p)

mkPatList :: Int -> [Patient]
mkPatList n =
    let l = map mkPat [1..n]
    in l `deepseq` l
    where
      mkPat i =
          Patient
          { p_firstName = "Stefan"
          , p_lastName = "Wehr"
          , p_note = "Dies ist ein bißchen mehr Text, aber auch nicht richtig lang"
          , p_age = i
          }

_NUM_ :: Int
_NUM_ = 100000

patList :: [Patient]
patList = mkPatList _NUM_

main :: IO ()
main =
    defaultMain
    [ env (return patList) $ \l ->
        bgroup "patList" $
        [ bench "safecopy" $ whnf (CH.hashMd5 . runPut . safePut) l
        , bench "cryptohash" $ whnf CH.hashMd5 l
        , bench "large-hashable (Manual)" $ whnf (LH.runLH LH.md5HashAlgorithm . LH.updateHashList updateHashPatient) l
        , bench "large-hashable (TH)" $ whnf (LH.largeHash LH.md5HashAlgorithm) l
        , bench "large-hashable (Generic)" $ whnf (LH.runLH LH.md5HashAlgorithm . LH.genericUpdateHash) l
        , bench "large-hashable-serial (TH)" $ whnf (Serial.serialLargeHash LH.md5HashAlgorithm) l
        ]
    ]
