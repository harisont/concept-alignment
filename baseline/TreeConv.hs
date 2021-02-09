module TreeConv    where

import Utils
import Data.List
import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

conll2deptree :: String -> DepTree
conll2deptree =
  depnodes2tree .
  map row2depnode .
  filter validDepNode .
  map (splitWithSep '\t') .
  filter ((/="#") . take 1) .
  lines

validDepNode ws = length ws > 7 && all isDigit (head ws)

data Tree n = T n [Tree n] deriving (Show,Eq,Ord)

root     :: Tree a -> a 
children :: Tree a -> [Tree a]
allnodes :: Tree a -> [a]
subtrees :: Tree a -> [Tree a]
root     (T n _)  = n
children (T _ ts) = ts
-- all nodes in a tree
allnodes t = root t : concatMap allnodes (children t)
-- all subtrees of a tree
subtrees t = t : concatMap subtrees (children t)

mapTree :: (n -> m) -> Tree n -> Tree m
mapTree f (T n ts) = T (f n) (map (mapTree f) ts)

sizeTree :: Tree n -> Int
sizeTree (T _ ts) = 1 + sum (map sizeTree ts)

-- foldTree :: (m -> m -> m) -> (n -> m) -> Tree n -> m -- can be used to implement span
-- elemTree     :: (n -> Int) -> Tree n -> Tree n -> Bool
-- notElemTree  :: (n -> Int) -> Tree n -> Tree n -> Bool

-- to normalize trees by sorting the subtrees based on label and distance. PK (idea from AR)
normalizeTree :: Ord a => (Tree n -> a) -> Tree n -> Tree n
normalizeTree key (T rt rts) = T rt (sortOn key (map (normalizeTree key) rts))

type MorphoTags = [(String,String)]   -- String
-- changed by PK on 2017-09-01
-- also changed hasMorpho, hasLemma, hasTagandMorpho functions
-- switched from plain string to list of tuples

data DepNode = DN {
  source   :: Int,
  word     :: String,
  lemma    :: String,
  postag   :: POS,
  xpostag  :: POS,         -- added later. helpful for use in PrepocUD.hs
  morpho   :: MorphoTags,
  position :: Int,
  label    :: Label,       -- udlabel found in the tree
  udlabel  :: Label,       -- udlabel without subtypes
  extdeps  :: String,
  misc     :: String,      -- called in CoNLL-U as misc
  status   :: String
  } deriving (Show,Eq,Ord)

initDepNode :: DepNode
initDepNode = DN 0 "" "" "" "" [] 0 "" "" "" "" ""

-- the distance of a node from its source; can be useful in sorting. AR 2018-08-29
distanceDepNode :: DepNode -> Int
distanceDepNode dn = abs (position dn - source dn)

type DepTree = Tree DepNode

-- Read the morph feature string and return
-- MorphoTags / [MorphoConstraints]
-- UD allows one feature to have multiple values, seperated by ,
-- e.g   ADV dit dit PronType=Int,Rel
morph2fs :: String -> MorphoTags
morph2fs = multVals . str2fsList '|' '=' "_" 
 where
  multVals fvs = [(f,v) | (f,vs)<-fvs, v<-splitWithSep ',' vs]

fs2morph :: MorphoTags -> String
fs2morph = lpairs2Str '|' '=' "_"

row2depnode :: [String] -> DepNode
row2depnode (positio:wor:lemm:posta:xpos:morph:sourc:labe:edep:mis:_) =
  initDepNode {word=wor,lemma=lemm,postag=posta,
    position = readInt positio,
    xpostag = xpos,          -- fine-grained POS
    morpho = morph2fs morph,
    source = readInt sourc,
    label = labe,
    udlabel = crselbl,
    extdeps = edep,
    misc = mis               -- comments no longer ignored
  }
  where
    crselbl = case break (==':') labe of {(ud, ':':subtype) -> ud ; (ud, "") -> ud}

-- This works for auto-parsed datasets when the root may not be identified
-- using the special "root" label but rather using the source of the node (0)
depnodes2tree :: [DepNode] -> DepTree
depnodes2tree dns = c2t root where
  root = case [dn | dn <- dns, source dn == 0] of
    [dn] -> dn
----    dn:_ -> dn ---- to recover from corrups treebank data
    _ -> error "no unique root"
  c2t dn = T dn (children dn)
  children dn = [c2t d | d <- dns, source d == position dn]

-- deprecated. this works for treebank trees where label=="root" => source==0
depnodes2tree_ :: [DepNode] -> DepTree
depnodes2tree_ dns = c2t root    where
  root = case [dn | dn <- dns, label dn == "root"] of
    [dn] -> dn
    _ -> error "no unique root"
  c2t dn = T dn (children dn)
  children dn = [c2t d | d <- dns, source d == position dn]

indent :: Int
indent = 4

prDepTree :: DepTree -> String
prDepTree = prTree prDepNode

-- indented tree without parentheses
prTree :: (n -> String) -> Tree n -> String
prTree prn = unlines . prt 0     where
  prt i (T n ts) = (replicate (i*indent) ' ' ++ prn n) : concatMap (prt (i+1)) ts

-- unindented tree with parentheses
prTreeShort :: (n -> String) -> Tree n -> String
prTreeShort prn (T n ts) = unwords (prn n : map prt ts)    where
  prt t@(T tn tns) = if (null tns) then (prn tn) else "(" ++ unwords (prn tn : map prt tns) ++ ")"

prDepNode :: DepNode -> String
prDepNode n = unwords [label n, lemma n, postag n, fs2morph (morpho n), show (position n), status n]

normalizeDepTree :: DepTree -> DepTree 
normalizeDepTree = normalizeTree dist    where -- use distance to order children 
  dist = \t -> distanceDepNode (root t)
  labanddist = \t -> (label (root t), distanceDepNode (root t))

type Fun   = String
type Cat   = String
type Var   = String     -- used in DefMap
type Label = String
type POS   = String
type Lang  = String     -- should no more be needed except in lexicon lookup

data GFNode = GN {
  fun  :: Fun,          -- GF function
  src  :: Int           -- the source word, default 0; used for counting coverage
  } deriving (Show,Eq,Ord)

type GFTree = Tree GFNode

initGFNode = GN "NONE" 0

initGFTree = T initGFNode []

prGFTree = prTreeShort prGFNode

prGFTreeLong = unlines . prt 0    where
  prt i (T n ts)
    | all (null . children) ts =
        [replicate (i*indent) ' ' ++ unwords (prGFNode n : map (prGFNode . root) ts)]    -- 1 line if all args are atomic
    | otherwise =
        (replicate (i*indent) ' ' ++ prGFNode n) : concatMap (prt (i+1)) ts

prGFNode n = fun n

-- rename [GFTree] as BackT (useful when adding backup trees for other gf tree candidates)
type BackT = [GFTree]
initBackT  = [] 

-- added backcands for each treecand  on 2017/05/03
data AbsNode = AN {
  gftree    :: GFTree,         -- the tree constructed; must always be a valid GF tree
  cat       :: Cat,            -- the type of this tree
  lab       :: Label,          -- original label, used in intermediate phases
  treecands :: [(GFTree,Cat)], -- local subtree candidates ---- used for lexical items only
  subcands  :: [(GFTree,Cat)], -- suboptimal subtree candidates (with coverage less than optimal)
                               -- these candidates are merged after the node is finished processing
  backtrees :: BackT,          -- collection of unused nodes for the top GF tree, if any
  backcands :: [BackT],        -- collection of unused nodes for all the rest of candidates
  tags      :: MorphoTags,     -- morphological tags
  udtag     :: POS,            -- UD tag. helpful for OOV word handling. Otherwise, not used
  lem       :: String,         -- lemma helpful for handling lexicalized configs
  positio   :: Int             --- 0 for non-lexical functions
  } deriving (Show,Eq,Ord)

initAbsNode :: AbsNode
initAbsNode = AN initGFTree "String" "dep" [] [] initBackT [] [] "" "" 0 -- rename default label to dep label to match gf2ud

type AbsTree = Tree AbsNode

-- add the actually 1-ranked tree to the other candidates
allTreecands an = (gftree an, cat an) : treecands an

-- retrive all the backups GF trees for the respective candidate trees
-- in hindsight, I figured out wrong use of this function was the reason why
-- backups were not properly being applied in the tree building stage.
-- so, this function is a bad idea, and its use should be avoided.
-- instead, the functions allTreesWithBkups should be used everywhere
-- for now, I will leave it in place in case it breaks other code. 
allBackcands an = backtrees an : backcands an

allTreesWithBkups an = 
  case (length (allTreecands an) == length (allBackcands an)) of
    True  -> [((gt,gc),bs) | ((gt,gc),bs) <- zip (allTreecands an) (allBackcands an)] -- if the backups have been built by this point
    False -> let 
               bcndspad = (backtrees an):backcands an ++ repeat initBackT             -- the backcands by default should be an empty list
             in [((gt,gc),bs) | ((gt,gc),bs) <- zip (allTreecands an) bcndspad]

cnstf f i    = initGFNode {fun=f, src=i}
cnst f i     = T (cnstf f i) []

apps f xs    = T (initGFNode{fun=f}) xs
app0 f       = apps f []
app1 f a     = apps f [a]
app2 f a b   = apps f [a,b]
app3 f a b c = apps f [a,b,c]

prAbsTree :: AbsTree -> String
prAbsTree = prTree prAbsNode

prAbsNode :: AbsNode -> String
prAbsNode n = unwords [
  lab n,
  udtag n,
  prGFTree (gftree n) ++ " : " ++ cat n,
  prTreecands (treecands n),
  prBacktrees (backtrees n),
  prCoveredNodes (gftree n),   -- added 2017/04/26
  "[" ++ show (length (allTreecands n)) ++ "/" ++ show (length (nub $ map snd $ allTreecands n)) ++ "]",
  show (positio n)
  ]

prBacktrees cs   = "{" ++ concat (intersperse "," [prGFTree t | t <- cs]) ++ "}"

prCoveredNodes t = "(" ++ concat (intersperse ","  [show n | n <- nub $ nodesUsedGen src t]) ++ ")"

prTreecands cs   = "[" ++ concat (intersperse ", " [prGFTree t ++ " : " ++ c | (t,c) <- cs]) ++ "]"

sortByNodesUsed :: [AbsTree] -> [AbsTree]
sortByNodesUsed ts = map snd $ reverse $ sort [(length (nodesUsedGen positio t),t) | t <- ts]

{-
-- depreceated.
sortByNodesUnUsed :: Int -> [AbsTree] -> [AbsTree]
sortByNodesUnUsed n ts = map snd $ sort [(n - (length (nodesUsedGen positio t)),t) | t <- ts]

sortByNodesCovered :: [GFTree] -> [GFTree]
sortByNodesCovered ts = map snd $ reverse $ sort [(length (nodesUsedGen src t),t) | t <- ts]
-}

nodesUsedGen :: (n -> Int) -> Tree n -> [Int]
nodesUsedGen posi ast = nub (nus ast)    where
  nus (T f ts) = (case posi f of 0 -> [] ; p -> [p]) ++ concatMap nus ts

filterTrees :: (n -> Bool) -> [Tree n] -> [Tree n] 
filterTrees predi asts = filter (predi . root) asts

-----------------------------------------------------
-- the first step from dep to abs: lexical annotation
-- this is the main algorithm in this module
-----------------------------------------------------

-- (1) change the datastructure, use quoted lemma as tree of type String
dep2absNode :: DepNode -> AbsNode
dep2absNode d = initAbsNode {
  gftree = T initGFNode{fun = quote (lemma d), src = position d} [],
  lab = label d,
  tags = morpho d,
  udtag = postag d,
  lem = lemma d,
  positio = position d
  }

-- (2) annotate each node with candidate lexical items
-- produces just one tree, where lexical ambiguities are stored in candidates at each node
deptree2abstreeLex :: Configuration -> Dictionary -> DepTree -> AbsTree
deptree2abstreeLex config dict dt@(T dn dts) = T (annot dn) (map annots dts) where

  annots = deptree2abstreeLex config dict
  annot d = absnode0{gftree = gft, cat = gfc, treecands = tail cands} where

    absnode0 = dep2absNode d  -- decorate absnode0 from the original depnode d

    (gft,gfc) = head cands

    cands0 = lookLex (lemma d) (postag d) (position d) (morpho d)
    cands1 = cands0 ++ [(gftree absnode0, cat absnode0)]             -- keep initial String as backup candidate
    cands2 = case filter ((/="String") . snd) cands1 of              -- remove String candidates if there are others
               cs@(_:_) -> cs                                        --- note: a String can sometimes be right,
               _ -> case coverOOV (head cands1) (postag d) (position d) (morpho d) of  --- e.g. when a word is taken literally or is missing from lexicon
                      cs@(_:_) -> cs
                      _ -> cands1
               -- _ -> cands1                                        -- e.g. when a word is taken literally as a name
    cands  = nub cands2

  udCatMap = categories config

  -- lexicon lookup for a lemma with certain POS and morphological tags
  lookLex lem pos i mor = concat $ maybe [] (map (mkLex lem i)) $ do -- this "do" block returns Maybe [(Cat,[Fun])]
    cis <- M.lookup pos udCatMap                                     -- find categories compatible with POS 
    let cs = [catid ci |               -- filter those categories that match morphological and lemma constraints
                ci <- cis,
                and [hasMorpho id mc mor && hasLemma mc lem | ("head",mc) <- mconstraints ci]
             ]
    let lclem = map toLower lem                                      -- lookup key is case-insensitive
    return [(c,fs) |                   -- for each such category, find the functions with this key and category
      c <- cs, let fs = maybe [] id $ M.lookup (lclem,c) dict
      ]

  -- after lookup, build a set of GF trees
  mkLex lem i (c,fs) = case fs of
    _:_ -> [(cnst f i, c) | f <- fs]                                 -- GF function constant with its category
    []  -> [(cnst (quote lem) i, "String")]                          -- unknown words are kept as strings

  -- OOV handling. Added 2017/04/04
  -- this function assumes the grammar and configs has functions of 
  -- StringX : String -> X    udtag=X   [morph.constraints]
  -- this should atleast place the lemma/string when generating the
  -- linearization as a literal while partially making use of category info.
  oldcoverOOV (gft,gfc) tag src mtag = alltrees    where
    strfuns  = [fi | fi <- (functions config),                       -- functions like MkSymb, which take String as a argument     
                       length (argtypes fi) == 1,                    -- unary functions only 
                       elem (gfc,"head") (argtypes fi) ]
    oovtrees = [(tree,cv) | fi <- strfuns,                           -- verify if morph. constraints are satisfied
                                and [hasTagandMorpho id mo tag mtag | ("head",mo) <- morphoconstraints fi],
                                let cv = valtype fi, 
                                let tree = app1 (funid fi) gft]      -- apply the function to the OOV word
    alltrees = oovtrees

  -- External lexicon handling. Added 2018/09/13
  -- this function takes the (lemma,postag) combination as face-value and
  -- returns a tree of one-more categories 
  coverOOV (gft,gfc) tag src mtag = alltrees    where
    getLem  gft       = unquote $ fun (root gft)
    funname (lem,cat) = "@"++(map (\x -> if x==' ' then '_' else x) lem)++"_"++cat -- external functions are maked with "_" as prefix
    strfuns  = [fi | fi <- (functions config),                       -- functions like MkSymb, which take String as a argument     
                       length (argtypes fi) == 1,                    -- unary functions only 
                       elem (gfc,"head") (argtypes fi)]
    oovtrees = [(tree,cv) | fi <- strfuns,                           -- verify if morph. constraints are satisfied
                                and [hasTagandMorpho id mo tag mtag | ("head",mo) <- morphoconstraints fi],
                                let cv = valtype fi,
                                let tree = app1 (funid fi) gft]      -- apply the function to the OOV word
    extlexts = [(tree,cv) | ci <- fromMaybe [] $ M.lookup tag udCatMap,
                             let lem=getLem gft,
                             and [hasMorpho id mc mtag && hasLemma mc lem | ("head",mc) <- mconstraints ci],
                             let cv=catid ci, 
                             let tree=cnst (funname (lem,cv)) src]
    alltrees = case oovtrees of {cs@(_:_) -> oovtrees ; _ -> extlexts}
    
type Lattice = M.Map (Int,Fun) [Fun]   -- simulate a confusion network using a Map table

prCNetwork :: Lattice -> String
prCNetwork lat = unlines (map prGrp grps)    where
  table = M.toAscList lat
  posL  = nub [p | ((p,_),ts) <- table]                                      -- ambiguous words in the sentence
  grps  = [(pos,[rf:rfs | ((p,rf),rfs) <- table, pos == p]) | pos <- posL]
  prGrp grp@(p,tsc) = show p ++ "\t" ++ concat (intersperse ", " [(concat $ intersperse "/" ts) | ts <- tsc])

-- (2.5) construct a lattice over lexical functions
--
buildLattice :: AbsTree -> (AbsTree, Lattice)
buildLattice at@(T an ats) =
  let
    lats1    = map buildLattice ats
    ats1     = map fst lats1
    lts1     = map snd lats1
    (an1,lt) = select an
    ltt      = M.unionsWith (++) (lt:lts1)
  in (T an1 ats1, ltt)
  where
    select an =
     let
       grpcands  = [(c, [gt | (gt,gc) <- allTreecands an,
                               gc == c] )
                      | c <- nub $ map snd (allTreecands an) ]                 -- for each unique category
       polyfuns  = [((positio an,fun gan), [fun rn | t <- cands, let rn = root t])   -- if there are more than one function
                                                                               -- (src,fun) used as key; cands as alternatives to fun
                      | (c,gts@(repr:cands)) <- grpcands,                      -- it should have atleast one candidate
                        not  (null cands),                                     -- only if the word is polysemous
                        null (children repr),                                  -- function must be a 0-arg function
                        let gan  = (root repr) ]
       lat       = M.fromList polyfuns                                         -- (1,"law_1_N") -> ["law_1_N", "law_2_N"]
       (t,c):tcs = [(gt,gc) | (gc,gts) <- grpcands, let gt = head gts]         -- same representative is chosen as proto-type
     in (an{gftree=t, cat=c, treecands=tcs}, lat)

--------------------------------------------------------------
---- configurations as data, read from a set of *.labels files
--------------------------------------------------------------

type CatMap  = M.Map POS [CatInfo]
type DefMap  = M.Map Fun ([Var],GFTree)
type FunsTbl = M.Map Cat [FunInfo]    -- for speedup; group functions based on head cat. added 2018/09/15

data Configuration = Conf {
  grammarname    :: String,          -- name of abstract syntax
  categories     :: CatMap,
  functions      :: [FunInfo],
  backups        :: [FunInfo],
  definitions    :: DefMap,
  helpcategories :: M.Map Cat Cat    -- added 2017/04/06 for handling sub-categories
--  startcat     :: Cat              -- added 2018/08/29 
  } deriving Show

data FunInfo = FI {
  funid    :: Fun,
  valtype  :: Cat,
  argtypes :: [(Cat,Label)],
  morphoconstraints :: [LMorphoConstraint]
  } deriving (Show,Eq,Ord)

data CatInfo = CI {
  catid        :: String,
  mconstraints :: [LMorphoConstraint]
  } deriving (Show,Eq,Ord)

---- switched from simple string to tuple
type MorphoConstraint  = (String,String)
---- allows for constraints like case.lemma="of"
---- changed on 2017/10/09
type LMorphoConstraint = (Label,MorphoConstraint)

-- check if a morphological constraint is among the morpho tags
hasMorpho :: (n -> MorphoTags) -> MorphoConstraint -> n -> Bool
hasMorpho mf ("lemma",_) node = True
hasMorpho mf c node = elem c (mf node)
--  | take 6 c == "lemma=" = True       -- this is a lemma constraint, not morpho
--  | otherwise = isInfixOf c (mf node) ---- use structure

-- a special case is constraint of form lemma=x
hasLemma :: MorphoConstraint -> String -> Bool
hasLemma (mf,mv) lemma = case mf of
  "lemma" -> lemma == mv
  _ -> True

-- added 2017/04/04
-- check if a morphological constraint is among the morpho tags
hasTagandMorpho :: (n -> MorphoTags) -> MorphoConstraint -> POS -> n -> Bool
hasTagandMorpho id mc pos node = case fst mc of
  "udtag" -> snd mc == pos
  _ -> hasMorpho id mc node
--  | take 6 c == "udtag=" = hasUDtag c pos
--  | otherwise = hasMorpho id c node

initFunInfo = FI "" "" [] []
initCatInfo = CI "" []

depArgTypes :: FunInfo -> [(Cat,Label)]
depArgTypes fi = [(c,l) | (c,l) <- argtypes fi, l /= "head"]

-- added 2018/09/15.
mkFunTbl :: [FunInfo] -> FunsTbl 
mkFunTbl funs = M.fromListWith (++) [(ca,[fi]) | fi<-funs, (ca,"head") <- argtypes fi]

-- don't know who put this here, but it is not used elsewhere
prFunInfos fis = "[" ++ unwords (map funid fis) ++ "]" 
-- maybe replace it with more detailed information 
-- like the one printed below ? 
prFunInfo fi  = unwords [funid fi, ":", sigtype, ";", lblconfig]     where 
  sigtype   = concat $ intersperse " -> " (map fst (argtypes fi) ++ [valtype fi])
  lblconfig = unwords (map snd (argtypes fi))

lexFunInfo :: Fun -> Cat -> FunInfo
lexFunInfo f c = initFunInfo {funid = f, valtype = c}

-------------

type Dictionary = M.Map (String,Cat) [Fun]  -- (word,cat) -> funs
emptyDictionary = M.empty

-- added on 2017/08/29 to handle variants if provided in an auxiliary dictionary
mergeDictionary :: Dictionary -> Dictionary -> Dictionary
mergeDictionary init var = M.unionWith (++) init var

-- added on 2017/11/07 to handle case-insensitive lookup
lcDictionary :: Dictionary -> Dictionary
lcDictionary dict = M.fromListWith (++) $ map (\ ((x,y),val) -> ((map toLower x,y),val) ) $ M.toList dict

------------------------

-- return all numbers and results wrt. to sentence as this packed object
-- added on 21/06/2018
data ConversionResult = CR {
    len    :: Int,
    cover  :: Int,
    tintp  :: Int,
    hintp  :: Int,
    nintp  :: Int,
    strscore :: Double,
    tlin :: [(String,String)],
    backfuns :: [Fun],
    undeffns :: [Fun],
    helpfuns :: [Fun],
    missfuns :: [FunInfo],
    blckcfgs :: [(Cat,[Fun])]    -- functions in grammar that cannot be reached with current configurations grouped by cat
  } deriving Show

initRes l = CR l 0 0 0 0 0.0 [] [] [] [] [] []

updateRes (l,c,ti,hi,ni) = CR {  len = l, 
                                 cover = c, 
                                 tintp = ti, hintp = hi, nintp = ni, 
                                 strscore = 0.0, 
                                 tlin     = [],
                                 backfuns = [], 
                                 undeffns = [], 
                                 helpfuns = [], 
                                 missfuns = [],
                                 blckcfgs = []
                              }

-- to collate information across individual results
mergeRes :: ConversionResult -> ConversionResult -> ConversionResult
mergeRes sc1 sc2 = CR {  len   = (len sc1)   + (len sc2),
                         cover = (cover sc1) + (cover sc2),
                         tintp = (tintp sc1) + (tintp sc2),
                         hintp = (hintp sc1) + (hintp sc2),
                         nintp = (nintp sc1) + (nintp sc2),
                         strscore = 0.0,
                         tlin     = (tlin sc1)     ++ (tlin sc2),
                         backfuns = (backfuns sc1) ++ (backfuns sc2),
                         undeffns = (undeffns sc1) ++ (undeffns sc2),
                         helpfuns = (helpfuns sc1) ++ (helpfuns sc2),
                         missfuns = (missfuns sc1) ++ (missfuns sc2),
                         blckcfgs = (blckcfgs sc1) ++ (blckcfgs sc2)
                      }

-- macro-scores across dataset.  
-- previously used and reported in Nodalida (2017)
prMacroAvrg :: [ConversionResult] -> String
prMacroAvrg crs = 
  let 
    num /. den  = (fromIntegral num / fromIntegral den)   
    cov  = round $ sum [100*(cover cr /. len cr) | cr <- crs] / fromIntegral (length crs)
    tint = round $ sum [100*(tintp cr /. len cr) | cr <- crs] / fromIntegral (length crs)
    nint = round $ 100 - sum [100*(nintp cr /. len cr) | cr <- crs] / fromIntegral (length crs)
    hint = round $ 100 - sum [100*(hintp cr /. len cr) | cr <- crs] / fromIntegral (length crs)
  in unwords [show  cov ++ "%", "nodes parsed", 
              show tint ++ "%", "tree interpreted",
              show nint ++ "%", "node backups",
              show hint ++ "%", "heads wrapped"
             ]

-- micro-scores across dataset.  
-- closer in interpretation to how scores are reported in dependency parsing
prStats :: ConversionResult -> String
prStats crs = 
  let 
    num /. den = (fromIntegral num / fromIntegral den)
    cov    = round $ 100 * (cover crs) /. (len crs)
    tint   = round $ 100 * (tintp crs) /. (len crs)
    nint   = 100 - round (100 * (nintp crs) /. (len crs))
    hint   = 100 - round (100 * (hintp crs) /. (len crs))
  in unwords [show  cov++"%", "nodes parsed", 
              show tint++"%", "tree interpreted",
              show nint++"%", "node backups",
              show hint++"%", "heads wrapped"
             ]

-- useful for individual results: counts of how much is covered
-- on collated results, these numbers are uninteresting
prResStat :: ConversionResult -> String
prResStat sc = unwords ["PARSED", show (cover sc), ",",
                         "T.INTERPRETED", show (tintp sc), ",",
                         "H.INTERPRETED", show (hintp sc), ",",
                         "N.INTERPRETED", show (nintp sc), "/",
                         "LENGTH", show (len sc)   
                       ]
prResCov :: ConversionResult -> String
prResCov sc = unwords ["PARSED", show (cover sc), "/", "LENGTH", show (len sc)]
prResInt :: ConversionResult -> String
prResInt sc = unwords ["T.INTERPRETED", show (tintp sc), ",",
                       "H.INTERPRETED", show (hintp sc), ",",
                       "N.INTERPRETED", show (nintp sc), "/",
                       "LENGTH", show (len sc)
                      ]

-- overview information obtained from converting a treebank/dataset
-- reports the following information 
-- -++ 1) distribution of which backups were used and how often
-- -++ 2) functions with missing linearization in grammar (grouped by value category)
-- -++ 3) functions/categories included in the grammar but blocked by the configs
-- -++ 4) distribution of syntactic contexts in which backups have been used 
prDebugInfo :: ConversionResult -> [String]
prDebugInfo crs = [bckfuns, undefns, blkcfgs, misfuns]  
  where
    bckfuns = unwords $ map (\(k,f) -> k++"/"++(show f)) $ ordcounter (backfuns crs)  -- backups   used in the corpus
    undefns = unwords $ map (\(k,f) -> k++"/"++(show f)) $ ordcounter (undeffns crs)  -- functions without linearization
    hlpfuns = ordcounter (helpfuns crs)                -- helper functions used (not very interesting)
    blkcfgs = concat (intersperse "\n" (map (\(c,fs)-> c ++ "\t" ++ unwords fs) $ filter (\(c,fs) -> length fs>1) (blckcfgs crs)))
    misfuns = concat (intersperse "\n" (map (\(a,b) -> show b++"\t"++prFunInfo a) (ordcounter (missfuns crs))))
    ordcounter lst = sortBy (\ (_,c1) (_,c2) -> compare c2 c1) $ M.toList $ M.fromListWith (+) $ map (\item -> (item,1)) lst

------------------------
--
---- auxiliaries

unlexBind :: String -> String
unlexBind s = case s of
  ' ':'&':'+':' ':cs -> unlexBind cs
  c:cs -> c:unlexBind cs
  _ -> s
