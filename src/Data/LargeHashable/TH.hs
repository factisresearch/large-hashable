{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.LargeHashable.TH (

    deriveLargeHashable, deriveLargeHashableCtx, deriveLargeHashableNoCtx
  , deriveLargeHashableCustomCtx

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
deriveLargeHashable n = reify n >>= \info ->
    case info of
        TyConI dec ->
            case dec of
#if MIN_VERSION_template_haskell(2,11,0)
                DataD context name tyvars _ cons _ ->
#else
                DataD context name tyvars cons _ ->
#endif
                    buildInstance (ConT name) context tyvars cons

#if MIN_VERSION_template_haskell(2,11,0)
                NewtypeD context name tyvars _ con _  ->
#else
                NewtypeD context name tyvars con _  ->
#endif
                    buildInstance (ConT name) context tyvars [con]
                _ -> fail $ notDeriveAbleErrorMsg n info
        FamilyI _ instDecs -> fmap concat $ forM instDecs $ \instDec ->
            case instDec of
#if MIN_VERSION_template_haskell(2,11,0)
                DataInstD context name types _ cons _ ->
#else
                DataInstD context name types cons _ ->
#endif
                    buildInstance (foldl AppT (ConT name) types) context [] cons
#if MIN_VERSION_template_haskell(2,11,0)
                NewtypeInstD context name types _ con _ ->
#else
                NewtypeInstD context name types con _ ->
#endif
                    buildInstance (foldl AppT (ConT name) types) context [] [con]
                _ -> fail $ notDeriveAbleErrorMsg n info
        _ -> fail $ notDeriveAbleErrorMsg n info

-- | Derive a 'LargeHashable' instance with extra constraints in the
-- context of the instance.
deriveLargeHashableCtx ::
       Name
    -> ([TypeQ] -> [PredQ])
       -- ^ Function mapping the type variables in the instance head to the additional constraints
    -> Q [Dec]
deriveLargeHashableCtx tyName extraPreds =
    deriveLargeHashableCustomCtx tyName mkCtx
    where
      mkCtx args oldCtx =
          oldCtx ++ extraPreds args

-- | Derive a 'LargeHashable' instance with no constraints in the context of the instance.
deriveLargeHashableNoCtx ::
       Name
    -> (Q [Dec])
deriveLargeHashableNoCtx tyName =
    deriveLargeHashableCustomCtx tyName (\_ _ -> [])

-- | Derive a 'LargeHashable' instance with a completely custom instance context.
deriveLargeHashableCustomCtx ::
       Name
    -> ([TypeQ] -> [PredQ] -> [PredQ])
       -- ^ Function mapping the type variables in the instance head and the
       -- constraints that would normally be generated to the constraints
       -- that should be generated.
    -> (Q [Dec])
deriveLargeHashableCustomCtx tyName extraPreds =
    do decs <- deriveLargeHashable tyName
       case decs of
#if MIN_VERSION_template_haskell(2,11,0)
         (InstanceD overlap ctx ty body : _) ->
#else
         (InstanceD ctx ty body : _) ->
#endif
             do let args = reverse (collectArgs ty)
                newCtx <- sequence (extraPreds (map return args) (map return ctx))
                -- _ <- fail ("args: " ++ show args ++", ty: " ++ show ty)
#if MIN_VERSION_template_haskell(2,11,0)
                return [InstanceD overlap newCtx ty body]
#else
                return [InstanceD newCtx ty body]
#endif
         _ ->
             error $
                 "Unexpected declarations returned by deriveLargeHashable: " ++ show (ppr decs)
    where
      collectArgs :: Type -> [Type]
      collectArgs outerTy =
          let loop ty =
                  case ty of
                    (AppT l r) ->
                        case l of
                          AppT _ _ -> r : loop l
                          _ -> [r]
                    _ -> []
          in case outerTy of
               AppT _ r -> loop r
               _ -> []

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
updateHashClause i con =
        clause [return patOfClause]
            (normalB $
                foldl sequenceExps
                    [| updateHash ($(litE . IntegerL $ i) :: CULong) |]
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
              ForallC _ _ c -> patternForCon c
#if MIN_VERSION_template_haskell(2,11,0)
              GadtC [n] types _ -> ConP n $ uniqueVarPats (length types)
              RecGadtC [n] varTypes _ -> ConP n $ uniqueVarPats (length varTypes)
              _ -> error $ "Constructor not supported: "++show con
#endif
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
    map (\v -> (ConT (toLargeHashableClass v)) `AppT` (VarT . varName $ v)) vars
    where
      toLargeHashableClass :: TyVarBndr -> Name
      toLargeHashableClass var =
        case var of
          (PlainTV _) -> ''LargeHashable
          (KindedTV _ (AppT (AppT ArrowT StarT) StarT)) -> ''LargeHashable'
          (KindedTV _ _) -> ''LargeHashable

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
        gen acc n = gen (concat $ map (\q -> map (\c -> c : q) ['a'..'z']) acc) (n - 1)
