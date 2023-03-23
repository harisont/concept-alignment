-- by aarne@digitalgrammars.com 2023

module FunctionAlign where

import qualified Data.Set as Set
import PGF

-- input: a set of pairs of GF trees, representing aligned (i.e. equivalent) trees
-- output: a set of pairs of functions of GF trees, where aligned subtrees are treated as variables

type AlignedTrees = Set.Set (Expr, Expr)

-- this returns a set of function pairs from a set of expression pairs
-- CId is the variable name used in the created functions
findFunctions :: CId -> AlignedTrees -> AlignedTrees
findFunctions var aligns = Set.foldr Set.union Set.empty (Set.map findFuns aligns)
  where
    findFuns (eng, fin) = Set.fromList [
      (feng, ffin)
        | (e, f) <- alignedSubtrees aligns eng fin,
          let feng = buildFun var e eng,
          let ffin = buildFun var f fin,
          feng /= ffin
      ]

-- finding both 1- and 2-place alignments based on currying
findUnaryAndBinary :: AlignedTrees -> AlignedTrees
findUnaryAndBinary als = 
  let
    als1 = findFunctions (mkCId "y") als
  in findFunctions (mkCId "x") (Set.union als1 als)


subtrees :: Expr -> [Expr]
subtrees e = case unApp e of
  Just (f, xs) -> e : concatMap subtrees xs
  _ -> case unAbs e of
    Just (_, _, body) -> subtrees body
    _ -> [e]


alignedSubtrees :: AlignedTrees -> Expr -> Expr -> [(Expr, Expr)]
alignedSubtrees alignments eng fin = [
  (e, f)
    | e <- drop 1 (subtrees eng),
      f <- drop 1 (subtrees fin),
      Set.member (e, f) alignments
  ]


buildFun :: CId -> Expr -> Expr -> Expr
buildFun var x e = mkAbs var (body e)
  where
    body e = case unApp e of
      Just (f, es) -> case break (==x) es of
        (aes, _:bes) -> mkApp f (aes ++ [mkApp var []] ++ bes)
        _ -> mkApp f (map body es)

      _ -> case unAbs e of
          Just (t, v, b) -> PGF.mkAbs t v (body b)
    --- a hack, because normal mkAbs requires Explicit :: BindType, which is not in scope
    mkAbs v exp = -- mkApp (mkCId "ABSTRACT") [mkApp v [], exp]
      maybe undefined id (readExpr ("\\" ++ showCId v ++ " -> " ++ showExpr [] exp))


----------- END of the essential functions: these are for demos --------------

-- printing alignments as equations between lambda terms: \x -> f x = \x -> g x
printAlignment :: (Expr, Expr) -> String
printAlignment (e, f) = showExpr [] e ++ " = "  ++ showExpr [] f

-- a minimal example: three given alignments. They result in
-- \x, y -> PossNP (UseN x) (DetCN the_Det y) = \x, y -> DetCN (GenNP (MassNP y)) (UseN x)
-- \x, y -> PossNP (UseN y) (DetCN the_Det x) = \x, y -> DetCN (GenNP (MassNP x)) (UseN y)
-- \x -> PossNP (UseN tail_N) (DetCN the_Det x) = \x -> DetCN (GenNP (MassNP x)) (UseN häntä_N)
-- \x -> PossNP (UseN x) (DetCN the_Det cat_N) = \x -> DetCN (GenNP (MassNP kissa_N)) (UseN x)

exampleAligns = Set.fromList [(eng, fin), (ew, fw), (et, ft)]
  where
    Just eng = readExpr "PossNP (UseN tail_N) (DetCN the_Det cat_N)"
    Just fin = readExpr "DetCN (GenNP (MassNP kissa_N)) (UseN häntä_N)"
    Just ew = readExpr "cat_N"
    Just fw = readExpr "kissa_N"
    Just et = readExpr "tail_N"
    Just ft = readExpr "häntä_N"

-- reading expression pairs from lines of the form ("tree_1", "tree_2") 
readExprs :: String -> [(Expr, Expr)]
readExprs s = case (read s :: (String, String)) of
  (a, b) -> case (readExpr a, readExpr b) of
    (Just ea, Just eb) -> [(ea, eb)]
    _ -> []

-- a small example, giving
main = do
  let als2 = findUnaryAndBinary exampleAligns
  mapM_ (putStrLn . printAlignment) (Set.toList als2)

-- a big example, for an example file "aligns.tmp" that contains lines of form ("tree_1", "tree_2") 
mainz = do
  efs <- readFile "aligns.tmp" >>= return . map readExprs . take 20000 . lines
  let als = Set.fromList (concat efs)
  let als2 = findUnaryAndBinary als
  mapM_ (putStrLn . printAlignment) als2

