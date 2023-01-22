{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module HsNixPkgs.HSScript.Internal.BuildMod.TH (deriveWalk) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad
import Data.Bifunctor
import Data.Functor
import Data.Maybe
import qualified Data.Set as S
import HsNixPkgs.HSScript.Internal.BuildMod.Types
import Language.Haskell.TH

idE :: Name -> Q Clause
idE cn =
  newName "x"
    <&> \x ->
      Clause
        [AsP x (RecP cn [])]
        (NormalB (VarE 'pure `AppE` VarE x))
        []

doB :: [Stmt] -> Exp -> Body
doB s e = NormalB (DoE Nothing (s ++ [NoBindS (VarE 'pure `AppE` e)]))

walkInfix :: S.Set Type -> Exp -> Type -> Type -> Q (Maybe (Exp -> Exp -> Exp))
walkInfix ign e l r =
  liftA2 (,) (walkE ign l) (walkE ign r) <&> \case
    (Nothing, Nothing) -> Nothing
    (Just e1, Nothing) ->
      Just
        ( \x1 x2 ->
            VarE 'fmap
              `AppE` InfixE Nothing e (Just x2)
              `AppE` (e1 `AppE` x1)
        )
    (Nothing, Just e2) ->
      Just
        ( \x1 x2 ->
            VarE 'fmap
              `AppE` InfixE (Just x1) e Nothing
              `AppE` (e2 `AppE` x2)
        )
    (Just e1, Just e2) ->
      Just
        ( \x1 x2 ->
            VarE 'liftA2
              `AppE` InfixE Nothing e Nothing
              `AppE` (e1 `AppE` x1)
              `AppE` (e2 `AppE` x2)
        )

tupleElem :: Type -> Maybe [Type]
tupleElem = go 0 []
  where
    go n es (TupleT nl)
      | nl == n = Just es
      | otherwise = Nothing
    go n es (l `AppT` r) = go (n + 1) (r : es) l
    go _ _ _ = Nothing

walkE :: S.Set Type -> Type -> Q (Maybe Exp)
walkE ign = procExp
  where
    procExp :: Type -> Q (Maybe Exp)
    procExp t
      | t `S.member` ign = pure Nothing
      | ConT cn <- t =
          reify cn >>= \case
            TyConI d -> case d of
              TySynD _ [] dest -> procExp dest
              DataD {} -> pure (Just (VarE 'walk))
              NewtypeD {} -> pure (Just (VarE 'walk))
              _ -> error ("Unsupported decl: " ++ show d)
            i -> error ("Unexpected info: " ++ show i)
      | ListT `AppT` et <- t =
          procExp et <&> fmap (VarE 'traverse `AppE`)
      | TupleT 0 <- t = pure Nothing
      | TupleT 2 `AppT` t1 `AppT` t2 <- t =
          walkInfix ign (ConE '(,)) t1 t2 >>= \case
            Just f ->
              liftA2
                ( \x1 x2 ->
                    Just
                      ( LamE
                          [TupP [VarP x1, VarP x2]]
                          (f (VarE x1) (VarE x2))
                      )
                )
                (newName "x")
                (newName "x")
            Nothing -> pure Nothing
      | (tupleElem -> Just cons) <- t = do
          pe <- traverse procExp cons
          if all isNothing pe
            then pure Nothing
            else do
              ns <- mapM (\e -> (,e) <$> newName "t") pe
              (stmt, tv) <-
                first catMaybes
                  <$> mapAndUnzipM
                    ( \case
                        (nt, Nothing) -> pure (Nothing, nt)
                        (nt, Just e) ->
                          newName "tv" <&> \tv ->
                            (Just (BindS (VarP tv) (e `AppE` VarE nt)), tv)
                    )
                    ns
              pure
                ( Just
                    ( LamE
                        [TupP (fmap (VarP . fst) ns)]
                        ( DoE
                            Nothing
                            ( stmt
                                ++ [ NoBindS
                                       ( VarE 'pure
                                           `AppE` TupE (fmap (Just . VarE) tv)
                                       )
                                   ]
                            )
                        )
                    )
                )
      | ConT cn `AppT` et <- t,
        cn == ''Maybe =
          procExp et <&> fmap (VarE 'traverse `AppE`)
      | otherwise = pure (Just (VarE 'walk))

doWalk :: (Name -> t -> Q (b1, (Maybe Stmt, b2))) -> ([b1] -> Pat) -> ([b2] -> Exp) -> [t] -> Q Clause
doWalk f fp fe =
  fmap
    ( (\(ps, (stmts, es)) -> Clause [fp ps] (doB stmts (fe es)) [])
        . second (first catMaybes . unzip)
    )
    . mapAndUnzipM (\e -> newName "x" >>= \x -> f x e)

deriveCon :: S.Set Type -> [Con] -> Q [Clause]
deriveCon ign =
  traverse
    ( \case
        NormalC cn [] ->
          pure
            ( Clause
                [ConP cn []]
                (NormalB (VarE 'pure `AppE` ConE cn))
                []
            )
        NormalC cn bt ->
          traverse (walkE ign . snd) bt >>= \pe ->
            if all isNothing pe
              then idE cn
              else
                doWalk
                  ( \x me ->
                      case me of
                        Just e ->
                          newName "v" <&> \v ->
                            (VarP x, (Just (BindS (VarP v) (e `AppE` VarE x)), VarE v))
                        Nothing -> pure (VarP x, (Nothing, VarE x))
                  )
                  (ConP cn)
                  (foldl AppE (ConE cn))
                  pe
        RecC cn bt ->
          traverse (\(fn, _, t) -> (fn,) <$> walkE ign t) bt >>= \pe ->
            if all (isNothing . snd) pe
              then idE cn
              else
                doWalk
                  ( \x (fn, me) ->
                      let pat = (fn, VarP x)
                       in case me of
                            Just e ->
                              newName "v" <&> \v ->
                                (pat, (Just (BindS (VarP v) (e `AppE` VarE x)), (fn, VarE v)))
                            Nothing -> pure (pat, (Nothing, (fn, VarE x)))
                  )
                  (RecP cn)
                  (RecConE cn)
                  pe
        InfixC (_, t1) cn (_, t2) ->
          walkInfix ign (ConE cn) t1 t2 >>= \case
            Just f ->
              liftA2
                ( \x1 x2 ->
                    Clause
                      [InfixP (VarP x1) cn (VarP x2)]
                      (NormalB (f (VarE x1) (VarE x2)))
                      []
                )
                (newName "x")
                (newName "x")
            Nothing ->
              newName "x" <&> \x ->
                Clause
                  [AsP x (InfixP WildP cn WildP)]
                  (NormalB (VarE x))
                  []
        c -> error ("Unsupported constructor: " ++ show c)
    )

deriveWalk :: Name -> [TypeQ] -> DecQ
deriveWalk n ignQ = do
  ign <- S.fromList <$> sequenceA ignQ
  ds <-
    reify n
      >>= ( \case
              TyConI (DataD [] _ [] Nothing cs _) -> deriveCon ign cs
              TyConI (DataD {}) -> error ("Unsupported data decl: " ++ show n)
              _ -> error "Only data declaration is allowed"
          )
  pure
    ( InstanceD
        Nothing
        []
        (ConT ''WalkName `AppT` ConT n)
        [FunD 'walk ds]
    )