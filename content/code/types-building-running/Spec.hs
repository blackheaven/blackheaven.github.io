{-# LANGUAGE OverloadedRecordDot #-}

module Spec (main) where

import Control.Monad (forM_)
import Data.List (foldl', intercalate, unfoldr)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Kata" $ do
    it "should fail" $
      True `shouldBe` False

data NodeLint = NodeList
  deriving stock (Eq, Show)

type LintResult = ()

runLint :: NodeLint -> Tree -> [LintResult]
runLint _ _ = [()]

data LintSpec = LintSpec
  { path :: String,
    lintSpec :: NodeLint
  }
  deriving stock (Eq, Show)

newtype Tree = TreeNode [(String, Tree)]
  deriving stock (Eq, Show)

lintNaive :: [LintSpec] -> Tree -> [LintResult]
lintNaive specs tree =
  [ result
    | spec <- specs,
      targetNode <- findNodes (splitPath spec.path) tree,
      result <- runLint spec.lintSpec targetNode
  ]
  where
    findNodes :: [String] -> Tree -> [Tree]
    findNodes ps' tree' =
      case (ps', tree') of
        ([], _) -> [tree']
        (p : ps, TreeNode ts) -> concatMap (findNodes ps . snd) $ filter ((== p) . fst) ts
    splitPath =
      unfoldr $
        \remaining ->
          case break (== '/') remaining of
            ("", _) -> Nothing
            (p, ps) -> Just (p, ps)

data LintRule = LintRule {subRules :: LintRules, lints :: [NodeLint]}
  deriving stock (Eq, Show)

newtype LintRules = LR {lintRules :: Map.Map String LintRule}
  deriving newtype (Eq, Show, Semigroup, Monoid)

buildRules :: [LintSpec] -> LintRules
buildRules = foldl' addNode mempty . map splitByPath
  where
    splitByPath spec = (splitPath spec.path, spec.lintSpec)
    splitPath =
      unfoldr $
        \remaining ->
          case break (== '/') remaining of
            ("", _) -> Nothing
            (p, ps) -> Just (p, ps)
    addNode :: LintRules -> ([String], NodeLint) -> LintRules
    addNode (LR rules) (path, lint) =
      LR $
        case path of
          [p] ->
            Map.alter
              (Just . maybe (LintRule mempty [lint]) (\rule -> rule {lints = lint : rule.lints}))
              p
              rules
          (p : ps) ->
            let addOn sn = addNode sn (ps, lint)
             in Map.alter
                  (Just . maybe (LintRule (addOn (LR mempty)) mempty) (\rule -> rule {subRules = addOn rule.subRules}))
                  p
                  rules

lintFromRules :: LintRules -> Tree -> [LintResult]
lintFromRules rules tree@(TreeNode nodes) = concatMap runSubRules nodes
  where
    runSubRules :: (String, Tree) -> [LintResult]
    runSubRules (nodeName, node) =
      let subRule = Map.lookup nodeName rules.lintRules
       in concatMap (flip runLint tree) (maybe mempty (.lints) subRule)
            ++ lintFromRules (maybe mempty (.subRules) subRule) node
