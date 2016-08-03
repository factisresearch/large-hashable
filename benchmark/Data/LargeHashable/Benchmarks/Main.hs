{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.LargeHashable.Benchmarks.Main (main) where

import qualified Data.Text as T
import Data.SafeCopy
import Control.DeepSeq
import GHC.Generics
import Data.Serialize.Put
import System.Environment
import System.Exit
import qualified Data.LargeHashable.Benchmarks.CryptoHash as CH
import qualified Data.LargeHashable.Benchmarks.Serial as Serial
import qualified Data.LargeHashable as LH
import qualified Data.Bytes.Serial as S

data Patient
    = Patient
    { p_firstName :: !T.Text
    , p_lastName :: !T.Text
    , p_note :: !T.Text
    , p_age :: !Int
    }
    deriving (Eq, Show, NFData, Generic)

$(deriveSafeCopy 0 'base ''Patient)

instance S.Serial Patient

instance CH.LargeHashable Patient where
    hashUpdate ctx0 p =
        let !ctx1 = CH.hashUpdate ctx0 (p_firstName p)
            !ctx2 = CH.hashUpdate ctx1 (p_lastName p)
            !ctx3 = CH.hashUpdate ctx2 (p_note p)
            !ctx4 = CH.hashUpdate ctx3 (p_age p)
        in ctx4

instance LH.LargeHashable Patient where
    updateHash p =
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
_NUM_ = 1000000

patList :: [Patient]
patList = mkPatList _NUM_

genOnly :: IO ()
genOnly =
    do let !l = patList
       putStrLn ("Generated " ++ show (length l) ++ " patients")

hashSafeCopy :: IO ()
hashSafeCopy =
    do let !l = patList
       putStrLn ("Generated " ++ show (length l) ++ " patients")
       let !bs = runPut (safePut l)
           !hash = CH.hashMd5 bs
       putStrLn ("Hash: " ++ show hash)

hashCryptoHash :: IO ()
hashCryptoHash =
    do let !l = patList
       putStrLn ("Generated " ++ show (length l) ++ " patients")
       let !hash = CH.hashMd5 l
       putStrLn ("Hash: " ++ show hash)

hashLargeHashable :: IO ()
hashLargeHashable =
    do let !l = patList
       putStrLn ("Generated " ++ show (length l) ++ " patients")
       let !hash = LH.largeHash LH.md5HashAlgorithm l
       putStrLn ("Hash: " ++ show hash)

hashSerial :: IO ()
hashSerial =
    do let !l = patList
       putStrLn ("Generated " ++ show (length l) ++ " patients")
       let !hash = Serial.serialLargeHash LH.md5HashAlgorithm l
       putStrLn ("Hash: " ++ show hash)

main :: IO ()
main =
    do args <- getArgs
       case args of
         ["dry"] -> genOnly
         ["safecopy"] -> hashSafeCopy
         ["cryptohash"] ->  hashCryptoHash
         ["large-hashable"] -> hashLargeHashable
         ["large-hashable-serial"] -> hashSerial
         _ ->
             do putStrLn ("invalid arguments: " ++ show args)
                exitWith (ExitFailure 1)
