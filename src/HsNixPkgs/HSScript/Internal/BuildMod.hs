{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HsNixPkgs.HSScript.Internal.BuildMod (buildMod) where

import Control.Applicative
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import HsNixPkgs.HSScript.Internal.BuildMod.TH
import HsNixPkgs.HSScript.Internal.BuildMod.Types
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.PprLib as P
import Language.Haskell.TH.Syntax

let ign =
      [ [t|Int|],
        [t|Char|],
        [t|String|],
        [t|Lit|],
        [t|Bang|],
        [t|Overlap|],
        --
        [t|Phases|],
        [t|RuleMatch|],
        [t|Callconv|],
        [t|Fixity|],
        [t|Inline|],
        [t|Role|],
        [t|Safety|],
        [t|Specificity|],
        [t|SumAlt|],
        [t|SumArity|],
        [t|TyLit|]
      ]
 in traverse
      (\(t, e) -> deriveWalk t (ign ++ e))
      [ (''Exp, []),
        (''Match, []),
        (''Clause, []),
        (''Pat, []),
        (''Stmt, []),
        (''Con, []),
        (''Type, []),
        (''Dec, []),
        (''FunDep, []),
        (''RuleBndr, []),
        (''TySynEqn, []),
        (''InjectivityAnn, []),
        (''DerivClause, []),
        (''DerivStrategy, []),
        (''AnnTarget, []),
        (''Body, []),
        (''FamilyResultSig, []),
        (''Foreign, []),
        (''Guard, []),
        (''PatSynArgs, []),
        (''PatSynDir, []),
        (''Pragma, []),
        (''Range, []),
        (''TypeFamilyHead, [])
      ]

walkTyVarBndr :: TyVarBndr flag -> RnM (TyVarBndr flag)
walkTyVarBndr (PlainTV n f) = (`PlainTV` f) <$> walk n
walkTyVarBndr (KindedTV n f k) = liftA2 (`KindedTV` f) (walk n) (walk k)

instance WalkName (TyVarBndr ()) where
  walk = walkTyVarBndr

instance WalkName (TyVarBndr Specificity) where
  walk = walkTyVarBndr

buildMod :: String -> [Extension] -> [Dec] -> (String, HS.HashSet String)
buildMod name ext ds =
  let (d, (p, _)) = runState (traverse walk ds) (HM.empty, 0)
   in ( show
          ( vcat
              ( concat
                  [ fmap (\e -> text "{-# LANGUAGE" <+> text (show e) <+> text "#-}") ext,
                    [ text "{-# LANGUAGE PackageImports #-}",
                      text "",
                      hsep [text "module", text name, text "where"],
                      text ""
                    ],
                    HM.foldrWithKey'
                      ( \(mp, m) v ->
                          ( hsep
                              [ text "import",
                                doubleQuotes (text (fromMaybe "this" mp)),
                                text "qualified",
                                text m,
                                text "as",
                                text v
                              ]
                              :
                          )
                      )
                      []
                      p
                  ]
              )
              $+$ text ""
              $+$ ppr d
          ),
        (HS.fromList . mapMaybe fst . HM.keys) p
      )