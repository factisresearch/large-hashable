{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.LargeHashable.TH (
    deriveLargeHashable
    ) where

import Data.LargeHashable.Class
import Language.Haskell.TH
import Foreign.C.Types (CULong (..))
import Control.Monad (forM)

-- | Template Haskell function to automatically derive
--   instances of 'LargeHashable'. The derived instances first
--   calls 'updateHash' with an unique identifier number for 
--   every constructor, followed by 'updateHash' calls for every
--   field of the constructor (if existent). It also works for
--   type families.
--
--   E. g. for the following code
--
--   @
--
-- data BlaFoo a = Foo
--               | Bar Int a        
--               | Baz a a
--
-- $(deriveLargeHashable ''BlaFoo)
--   @
--
--   The following instance gets generated:
--
--   @
-- instance LargeHashable a_apg8 =>
--         LargeHashable (BlaFoo a_apg8) where
--  updateHash Foo = updateHash (0 :: Foreign.C.Types.CULong)
--  updateHash (Bar a b)
--    = (((updateHash (1 :: Foreign.C.Types.CULong)) >> (updateHash a))
--       >> (updateHash b))
--  updateHash (XY a b)
--    = (((updateHash (2 :: Foreign.C.Types.CULong)) >> (updateHash a))
--       >> (updateHash b))
--    @
deriveLargeHashable :: Name -> Q [Dec]
deriveLargeHashable name = reify name >>= \info ->
    case info of
        TyConI dec ->
            case dec of
                DataD context name tyvars cons _ ->
                    buildInstance (ConT name) context tyvars cons
                NewtypeD context name tyvars con _  ->
                    buildInstance (ConT name) context tyvars [con]
                _ -> fail $ notDeriveAbleErrorMsg name info
        FamilyI _ instDecs -> fmap concat $ forM instDecs $ \instDec ->
            case instDec of
                DataInstD context name types cons _ ->
                    buildInstance (foldl AppT (ConT name) types) context [] cons
                NewtypeInstD context name types con _ ->
                    buildInstance (foldl AppT (ConT name) types) context [] [con]
                _ -> fail $ notDeriveAbleErrorMsg name info
        _ -> fail $ notDeriveAbleErrorMsg name info

-- | Generates the error message displayed when somebody tries to let us
--   derive impossible instances!
notDeriveAbleErrorMsg :: Name -> Info -> String
notDeriveAbleErrorMsg name info = "Could not derive LargeHashable instance for "
    ++ (show name) ++ "(" ++ (show info) ++ "). If you think this should be possible, file an issue."

-- | After 'deriveLargeHashable' has matched all the important information
--   this function gets called to build the instance declaration.
buildInstance :: Type -> Cxt -> [TyVarBndr] -> [Con] ->  Q [Dec]
buildInstance basicType context vars cons =
    let consWithIds = zip [0..] cons
        constraints = makeConstraints context vars
        typeWithVars = foldl appT (return basicType) $ map (varT . varName) vars
      in (:[]) <$> instanceD constraints (conT ''LargeHashable `appT` typeWithVars)
            [updateHashDeclaration consWithIds]

-- | This function generates the declaration for the 'updateHash' function
--   of the 'LargeHashable' typeclass. By taking the constructors with there
--   selected IDs and calling 'updateHashClause' for everyone of them to generate
--   the corresponding clause.
updateHashDeclaration :: [(Integer, Con)] -> Q Dec
updateHashDeclaration consWIds = funD 'updateHash (map (uncurry updateHashClause) consWIds)

-- | 'updateHashClause' generates a clause of the 'updateHash' function.
--   It makes sure all the fields are matched correctly and updates the hash
--   with the neccessary information about the constructor (its ID) and all 
--   of its fields.
updateHashClause :: Integer -> Con -> Q Clause
updateHashClause id con =
        clause [return patOfClause]
            (normalB $
                foldl sequenceExps
                    [| updateHash ($(litE . IntegerL $ id) :: CULong) |]
                    hashUpdatesOfConFields)
            []
    where hashUpdatesOfConFields = map (\name -> [| updateHash $(varE name) |]) patVarNames
          -- Extract the names of all the
          -- pattern variables from usedPat.
          patVarNames = case patOfClause of
                          ConP _ vars -> map (\(VarP v) -> v) vars
                          InfixP (VarP v1) _ (VarP v2) -> [v1, v2]
                          _ -> error "Pattern in patVarNames not matched!"
          patOfClause = patternForCon con

-- | Generate a Pattern that matches the supplied constructor
--   and all of its fields.
patternForCon :: Con -> Pat
patternForCon con = case con of
              NormalC n types -> ConP n $ uniqueVarPats (length types)
              RecC n varTypes -> ConP n $ uniqueVarPats (length varTypes)
              InfixC _ n _ -> InfixP (VarP . mkName $ "x") n (VarP . mkName $ "y")
              ForallC _ _ con -> patternForCon con
    where uniqueVarPats n = take n . map (VarP . mkName) $ names

-- | Sequences two Expressions using the '(>>)' operator.
sequenceExps :: Q Exp -> Q Exp -> Q Exp
sequenceExps first second = infixE (Just first) (varE '(>>)) (Just second)

-- | Generates the constraints needed for the declaration of
--   the 'LargeHashable' class. This means that the constraint
--   @LargeHashable $TypeVar$@ is added for every type variable
--   the type has.
makeConstraints :: Cxt -> [TyVarBndr] -> Q Cxt
makeConstraints context vars = return $ context ++
    map (\v -> (ConT ''LargeHashable) `AppT` (VarT . varName $ v)) vars

-- | Returns the 'Name' for a type variable.
varName :: TyVarBndr -> Name
varName (PlainTV n) = n
varName (KindedTV n _) = n

-- | An infinite list of unique names that
--   are used in the generations of patterns.
names :: [String]
names = concat $ map (gen (map (:[]) ['a'..'z'])) [0..]
  where gen :: [String] -> Integer -> [String]
        gen acc 0 = acc
        gen acc n = gen (concat $ map (\n -> map (\c -> c : n) ['a'..'z']) acc) (n - 1)
