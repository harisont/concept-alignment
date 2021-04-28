module ConceptAlignment where

import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.MultiSet as MS
import qualified Data.Set as S
import qualified Data.Map as M
import RTree
import UDConcepts
import UDPatterns

{- Basic (mostly data types) definitions and instances for alignments etc. -}

-- | An alignment is a pair of corresponding UD (sub)trees associated to some 
-- metadata 
data Alignment = A {
  trees :: AlignedTrees,    -- ^ pair of UD subtrees
  meta :: Meta              -- ^ metadata
} deriving (Show,Read,Eq,Ord)

-- | Initialize a potential alignment given just the pair of trees
initAlignment :: AlignedTrees -> Alignment
initAlignment tu = A {
  trees = tu,
  meta = initMeta
}

-- | Convert an alignment into a (key,value) pair, where the key is the tree
-- pair and the "value" is the metadata
alignment2keyval :: Alignment -> (AlignedTrees,Meta)
alignment2keyval a = (trees a,meta a)

-- | Convert a (key,value) pair, where the key is the tree pair and the 
-- "value" is the metadata, into an Alignment
keyval2alignment :: (AlignedTrees,Meta) -> Alignment
keyval2alignment (k,v) = A {
  trees = k,
  meta = v
}

-- | Left (source language) tree of an alignment
sl :: Alignment -> UDTree
sl a = let (AT (t,u)) = trees a in t

-- | Right (target language) tree of an alignment
tl :: Alignment -> UDTree
tl a = let (AT (t,u)) = trees a in u

-- | prAlignment is used instead of show for inspecting the alignments in an
-- easier way, e.g. in EvAlign and for debugging
prAlignment :: Alignment -> String
prAlignment a = prTrees a 
                ++ " reasons: " ++ show (reasons $ meta a)
                ++ " sentence IDs: " ++ show (sentIds $ meta a)
                ++ " n. occurrences: " ++ show (nOccurrences $ meta a)
  where prTrees a = linearize (sl a) ++ " ||| " ++ linearize (tl a)

-- | Check if an alignment "contains" another;
-- used both in pruning and selection of alignments for MT
contains :: Alignment -> Alignment -> Bool
a `contains` b = (sl b `isSubRTree` sl a) && (tl b `isSubRTree` tl a)

-- | AlignedTrees basically represents a pair of UD trees, but it is a newtype
-- due to the custom implementation of (==) 
newtype AlignedTrees = AT (UDTree,UDTree)
  deriving (Show,Read)

-- | Two pairs of aligned trees are considered equal whenever their
-- linearizations are the same
instance Eq AlignedTrees where
  AT (t1,u1) == AT (t2,u2) = linearize t1 == linearize t2 
                          && linearize u1 == linearize u2

-- | Aligned trees can be ordered based on:
-- 1. size of their left tree
-- 2. size of their right tree
-- 3. "linearization" of their left tree (alphabetically)
-- 4. "linearization" of their right tree (alphabetically)
instance Ord AlignedTrees where
  a <= b = k a <= k b
    where 
      k x = (depthRTree sla,depthRTree tla,linearize sla,linearize tla)
        where AT (sla,tla) = x

-- | The metadata of an alignment are:
data Meta = M {
  reasons :: S.Set Reason,  -- ^ reasons for alignment
  sentIds :: S.Set Int,     -- ^ ids of the sentences it was inferred from
  nOccurrences :: Int       -- ^ total number of occurrences
} deriving (Show,Read,Eq,Ord)

-- | Initialize metadata with default vals
initMeta :: Meta
initMeta = M {
  reasons = S.empty,
  sentIds = S.empty,
  nOccurrences = 1
}


{- Helper functions for alignment maps-}

-- | Combine the metadata of two (equivalent) alignments
combineMeta :: Meta -> Meta -> Meta
m `combineMeta` n = M {
  reasons = reasons m `S.union` reasons n,
  sentIds = sentIds m `S.union` sentIds n,
  nOccurrences = nOccurrences m + nOccurrences n
}

-- TODO: docs
insert' :: (AlignedTrees,Meta) -> M.Map AlignedTrees Meta -> M.Map AlignedTrees Meta 
insert' (at,m) = M.insertWith combineMeta at m

union' :: M.Map AlignedTrees Meta -> M.Map AlignedTrees Meta -> M.Map AlignedTrees Meta 
union' = M.unionWith combineMeta

unions' :: [M.Map AlignedTrees Meta] -> M.Map AlignedTrees Meta
unions' = M.unionsWith combineMeta
  
-- | Linearize a UD tree ignoring initial whitespace
-- TODO: move to gfud
linearize :: UDTree -> String
linearize = dropWhile (== ' ') . prUDTreeString

-- | Type of manual annotations.
-- An alignment can be incorrect (-), correct in the specific context where it 
-- is found (=) or correct and potentially reusable, at least in the same 
-- domain (+)
data Annotation = Incorrect | Specific | Correct
  deriving Eq

instance Show Annotation where
  show Incorrect = "-"
  show Specific = "="
  show Correct = "+"

instance Read Annotation where
  readsPrec _ "-" = [(Incorrect, "")]
  readsPrec _ "=" = [(Specific, "")]
  readsPrec _ "+" = [(Correct, "")]
  readsPrec _ a = error $ "annotation error: " ++ show a

-- | Alignments can be associated with annotations
type Alignment' = (Annotation,Alignment)

-- | Reasons for alignment
data Reason = DIV   -- known interlingual divergence
            | UD    -- matching root UD label
            | POS   -- POS-equivalence
            | PASS  -- different voice, used together with DIV
            | CL    -- due to clause segmentation and alignment
            | REST  -- due to "alignment by exclusion"
            | HEAD  -- composed of the heads of another alignment (alignHeads)
            | PM    -- obtained via pattern matching/replacement pattern
            | PREV  -- already found in another sentence 
            | FAST  -- found by fast_align
  deriving (Eq,Show,Read,Ord,Enum,Bounded)

-- | Data type for alignment criteria. Each criterion is composed of
data Criterion = C {
  -- | a function telling when two dep. trees should be aligned
  func :: UDTree -> UDTree -> Bool, 
  -- | the reasons associated with such criterion
  reas :: S.Set Reason,
  -- | a flag telling whether heads should also be aligned (i.e. if alignSent
  -- should call alignHeads)
  headAlign :: Bool,
  -- | a flag telling wether the criterion is considered to be "strict",
  -- i.e. in practice if it should be used also for "alignment by exclusion" 
  strict :: Bool
}

-- | Special criterion necessary for effective clause segmentation
-- (that's why it is defined here and not in module Criteria)
clause :: Criterion
clause = C (\t u -> divClause t u || divClause u t) (S.singleton CL) True False
  where 
    -- a clause of a certain type is translated as a clause of another type 
    divClause :: UDTree -> UDTree -> Bool
    divClause t u = or [t `isLabelled` r | r <- clDEPRELs] 
                 && or [u `isLabelled` r | r <- clDEPRELs]

{- Alignment functions -}

-- | Align a list of pairs of / corresponding / dependency trees   
align :: S.Set Alignment           -- ^ a set of known alignments (e.g. from 
                                   -- statistical tools)
      -> [Criterion]               -- ^ a list of criteria (sorted by 
                                   -- priority)
      -> Maybe UDPattern           -- ^ a gf-ud pattern
      -> Bool                      -- ^ a flag indicating whether clause 
                                   -- segmentation should be performed 
      -> Bool                      -- ^ a flag indicating whether alignment  
                                   -- "by exclusion" should also be performed 
      -> [(UDSentence,UDSentence)] -- ^ the list of sentences to align 
      -> S.Set Alignment           -- ^ a set of alignments
align as _ _ _ _ [] = as
align as cs p cl ex (s:ss) = align (S.fromList $ map keyval2alignment (M.toList $ alignSent as' cs p cl ex s)) cs p cl ex ss
  where as' = M.fromListWith combineMeta (S.toList $ S.map alignment2keyval as)

-- | Sentence-level alignment function. Can be use independently of align   
alignSent :: M.Map AlignedTrees Meta   -- ^ a map of known alignments (e.g. 
                                       -- from statistical tools)
          -> [Criterion]               -- ^ a list of criteria (sorted by 
                                       -- priority)
          -> Maybe UDPattern           -- ^ a gf-ud pattern
          -> Bool                      -- ^ a flag indicating whether clause 
                                       -- segmentation should be performed 
          -> Bool                      -- ^ a flag indicating whether   
                                       -- alignment "by exclusion" should also 
                                       -- be performed 
          -> (UDSentence,UDSentence)   -- ^ the sentences to align 
          -> M.Map AlignedTrees Meta   -- ^ a map of alignments
alignSent as cs p cl ex s@(s1,s2) = if ex then extra else basic --TODO: extra `union` basic?
  where 
    basic = if cl then alignClauses (t,u) else alignSent' cs (t,u)
    extra = alignRest basic -- alignments obtained "by exclusion"
    id = if sentId s1 == sentId s2 
              then sentId s1 
              else error "unaligned sentences"
    (t,u) = (udSentence2tree s1, udSentence2tree s2)

    -- the list of criteria is needed because of how alignClause works
    alignSent' :: [Criterion] -> (UDTree,UDTree) ->M.Map AlignedTrees Meta
    alignSent' cs (t@(RTree n ts),u@(RTree m us)) = if (not . null) matchingCs
      then case (isJust p,headAlign (head matchingCs)) of
        -- not using a gf-ud pattern
        (False,True) -> insert' (alignment2keyval h) (insert' (alignment2keyval a) as) `union'` as'
        (False,False) -> insert' (alignment2keyval a) as `union'` as'
        -- using a gf-ud pattern: ignore head alignment altogether
        (True,_) -> if isJust m 
          then insert' (alignment2keyval $ fromJust m) (as `union'` as')
          else as `union'` as'
      else M.empty
      where 
        a = (initAlignment $ AT (t,u)) { 
          meta = initMeta {
            reasons = S.unions $ map reas matchingCs,
            sentIds = S.singleton id 
          }
        }
        h = alignHeads a -- TODO: add HEAD to reasons in alignHead
        m = alignPattern (fromJust p) a
        -- subtree alignments
        as' = unions' $ map (alignSent' cs) [(t,u) | t <- ts', u <- us']
            where (ts',us') = (sortByLabel ts,sortByLabel us)
                    where sortByLabel = sortOn (udSimpleDEPREL . root)
        matchingCs = 
          map snd (filter fst (zip [f t u | f <- map func cs] cs))

    -- call alignSent' on all pairs of clauses found in t and u
    alignClauses :: (UDTree,UDTree) ->M.Map AlignedTrees Meta
    alignClauses (t,u) = 
        unions' $ map (alignSent' (cs ++ [clause])) clausePairs
      where 
        clausePairs = [(ct,cu) | ct <- cts, cu <- cus, 
                                 length (clauses ct) == length (clauses cu)]
        -- get all clauses, sorted by dep. relation and distance from root
        (cts,cus) = (sortCs $ clauses t, sortCs $ clauses u)
        sortCs = sortOn (\(RTree n ts) -> 
          (udSimpleDEPREL n,dependencyDistance n))

    -- call alignSent' on all pairs of unaligned nominals + modifiers
    -- ("alignment by exclusion")
    alignRest :: M.Map AlignedTrees Meta -> M.Map AlignedTrees Meta
    alignRest as = 
      unions' $ map (alignSent' cs') nomPairs
        where
          nomPairs = [(nt,nu) | nt <- nts, nt `notElem` las, 
                                nu <- nus, nu `notElem` ras,
                                length (nommods nt) == length (nommods nu)]
          -- get all nominals and modifiers, sorted by label 
          -- and distance from root
          (nts,nus) = (sortNs $ nommods t, sortNs $ nommods u)
          sortNs = sortOn (\(RTree n ts) -> 
            (udSimpleDEPREL n,dependencyDistance n))
          (las,ras) = (map (\((AT (t,u)),v) -> t) (M.toList as), map (\((AT (t,u)),v) -> u) (M.toList as))
          cs' = map 
            (\(C f _ h s) -> C f (S.singleton REST) h s) 
            (filter strict cs)

    -- check if an alignment matches a certain gf-ud pattern.
    -- TODO: now it can become a boolean function: no replacement is done
    alignPattern :: UDPattern -> Alignment -> Maybe Alignment
    alignPattern p a = 
      case (ifMatchUDPattern p (sl a),ifMatchUDPattern p (tl a)) of
        -- only return an alignment if the pattern applies to both members
        (True,True) -> Just a
        _ -> Nothing

-- | Helper function for head alignment: given an alignment, return a new one  
-- for their "heads", respecting any compounds and aux+verbs (and more?)
-- TODO: mv inside
alignHeads :: Alignment -> Alignment
alignHeads a
  -- if there are compound constructions, look for their counterparts and
  -- align accordingly
  | (not . null) cts = 
      initHeadAlignment $ AT (RTree n cts, RTree m (compCounterparts ts us))
  | (not . null) cus = 
      initHeadAlignment $ AT (RTree n (compCounterparts us ts), RTree m cus)
  -- if the roots are verbs (to avoid messing with copulas) and only one of
  -- them has 1+ auxiliaries, align verb | verb + auxiliaries
  | all isVerb [n,m] && (not . null) ats && null aus = 
      initHeadAlignment $ AT (RTree n ats, RTree m [])
  | all isVerb [n,m] && null ats && (not . null) aus = 
      initHeadAlignment $ AT (RTree n [], RTree m aus) 
  | otherwise = initHeadAlignment $ AT (RTree n [], RTree m [])
  where
    AT (RTree n ts,RTree m us) = trees a
    -- select subtrees labelled in a certain way 
    filterByLabel l xs = filter (`isLabelled` l) xs

    ats = filterByLabel "aux" ts
    aus = filterByLabel "aux" us
    cts = filterByLabel "compound" ts
    cus = filterByLabel "compound" us

    -- TODO: add other reasons
    initHeadAlignment tu = a { meta = (meta a) { reasons = S.singleton HEAD } }
      where a = initAlignment tu

    -- given two lists of subtrees, select those that, in the second,
    -- could correspond to a compound construction in the first, i.e.
    compCounterparts :: [UDTree] -> [UDTree] -> [UDTree]
    compCounterparts ts us = cus ++ fus' ++ nus' ++ aus'
      where
        cus = filterByLabel "compound" us
        fus' = if length fus > length fts then fus else []
          where 
            fus = filterByLabel "flat" us
            fts = filterByLabel "flat" ts
        nus' = if length nus > length nts then nus else []
          where
            nus = filterByLabel "nmod" us
            nts = filterByLabel "nmod" ts
        aus' = if length aus > length ats then aus else []
          where
            aus = filterByLabel "amod" us
            ats = filterByLabel "amod" ts 

{- Propagation functions -}

-- | Generic (not optimized for same text in n languages) propagation function
--propagate :: [Criterion]            -- ^ a list of criteria 
--                                    -- (sorted by priority)
--          -> Bool                   -- ^ -- ^ a flag indicating whether clause 
--                                    -- segmentation should be performed
--          -> Bool                   -- ^ a flag indicating whether alignment
--                                    --"by exclusion" should also be performed 
--          -> ([UDTree],[UDTree])    -- a pair of lists of UD trees (the
--                                    -- sentences to propagate on) in L1, L2
--          -> UDTree                 -- a previously extracted L1 concept
--          -> Maybe (Alignment,Info) -- an (Alignment,Info) pair, if found
--propagate cs segment byExcl ([],_) _ = Nothing 
--propagate cs segment byExcl (t:ts,u:us) c =
--  let 
--    as = M.toList $ alignSent M.empty [] cs Nothing segment byExcl (t,u)
--    as' = case (c `isSubUDTree'` t, c `isHeadSubUDTree` t) of
--      (True,_) -> sortOnDepth as
--      (_,True) -> sortOnDepth (filter (\(_,(rs,_)) -> HEAD `elem` rs) as)
--      (False,False) -> []
--    in case find (\(a,_) -> c =~ sl a) as' of
--      Nothing -> propagate cs segment byExcl (ts,us) c
--      n -> n
--    where 
--      -- difference between the depth of the SL aligned subtree and c:
--      -- the smaller, the better (mostly used to avoid that root nodes are
--      -- aligned with full sentences, but also makes sense in general)
--      depthDiff :: RTree a -> Int
--      depthDiff t = abs (depthRTree t - depthRTree c)
--      sortOnDepth :: [(Alignment,Info)] ->[(Alignment,Info)]
--      sortOnDepth = sortOn (depthDiff . sl . fst)
--      -- check if c is the head of one of t's subtrees
--      isHeadSubUDTree :: UDTree -> UDTree -> Bool
--      isHeadSubUDTree c t = 
--        isJust $ listToMaybe $ 
--          sortOn depthDiff (filter (isHeadUDTree c) (allSubRTrees t))

---- check if a UD tree is the head of another
--isHeadUDTree :: UDTree -> UDTree -> Bool
--isHeadUDTree (RTree n []) (RTree m _) = n =~ m
--isHeadUDTree (RTree n ts) (RTree m _) = n =~ m && (hasAuxOnly || hasCompOnly) 
--  where 
--    hasAuxOnly = all (`isLabelled` "aux") ts
--    hasCompOnly = all 
--      (\t -> 
--        udSimpleDEPREL (root t) `elem` ["compound", "flat", "nmod", "amod"]
--      ) 
--      ts

{- POS-utils -}

-- | Multiset of the POS tags contained in a dep. tree
contentTags :: UDTree -> MS.MultiSet POS
contentTags = MS.fromList . filter relevant . map udUPOS . allNodesRTree
  where relevant p = p `elem` openPOS ++ ["NUM"] 

-- | Multiset of content tags of the subtrees of t
subtreesTags :: UDTree -> MS.MultiSet POS
subtreesTags t = MS.unions (map contentTags (subtrees t))

-- | POS: open classes
openPOS :: [POS]
openPOS = ["ADJ", "ADV", "INTJ", "NOUN", "PROPN", "VERB"]

-- | Check if a node is a verb
isVerb :: UDWord -> Bool
isVerb h = udUPOS h == "VERB" || udUPOS h == "AUX"


{- UD-utils -}

-- | Check if a sentence contains a clause in passive voice
isPassive :: UDTree -> Bool
isPassive s = 
  "pass" `elem` map (last . splitOn ":" . udDEPREL) (allNodesRTree s)

-- | Check if the label of the root of a tree is l
isLabelled :: UDTree -> Label -> Bool
isLabelled = flip isLabelled'
  where isLabelled' l = (== l) . udSimpleDEPREL . root

-- | Give a word, return its label / without subtypes /
udSimpleDEPREL :: UDWord -> Label
udSimpleDEPREL w = case break (==':') (udDEPREL w) of 
  (ud, ':':subtype) -> ud
  (ud, "") -> ud

-- | Given a sentence tree s, return the list of its clauses (s included)
clauses :: UDTree -> [UDTree]
clauses s = 
  s:[c | c <- allSubRTrees s, or [c `isLabelled` r | r <- clDEPRELs]]

-- | Given a UD tree, return the list of its nominals and modifiers
nommods :: UDTree -> [UDTree]
nommods t = [n | n <- allSubRTrees t, 
                      or [n `isLabelled` r | r <- nomDEPRELs ++ modDEPRELs]]

clDEPRELs, nomDEPRELs, modDEPRELs :: [Label]
clDEPRELs = ["csubj", "ccomp", "xcomp", "advcl", "acl"]  -- clauses
nomDEPRELs = ["nsubj", "obj", "iobj", "obl", "vocative", -- nominals
              "expl", "dislocated", "nmod", "appos", "nummod"]
modDEPRELs = ["advmod", "amod", "discourse"]             -- modifiers

-- | Construct an "abstract" UD tree (same shape, labels without subtypes 
-- as nodes)
abstractUDTree :: UDTree -> RTree Label
abstractUDTree = mapRTree udSimpleDEPREL


{- Alignments to CoNLL-U files -}

-- | convert an alignment into a pair of CoNNL-U sentences
alignment2sentencePair :: Alignment -> (UDSentence, UDSentence)
alignment2sentencePair a = 
  (udTree2adjustedSentence $ sl a, udTree2adjustedSentence $ tl a)
  where 
    udTree2adjustedSentence = adjustUDIds . udTree2sentence . createRoot
-- TODO: add each sentence its metadata in udCommentLines


{- Selection of alignments for MT -}

-- | Select the alignments relevant for MT. Namely:
-- - remove anything that does not contain any content word
-- - remove redundant alignments (i.e. alignments that can be inferred
--   from their sub-alignments)
-- - optionally, remove alignments where the size of both trees is > n
--selectForMT :: Maybe Int -> [(Alignment,Info)] -> [(Alignment,Info)]
--selectForMT mmax as = nubBy 
--                  (\(a,_) (b,_) -> b `contains` a && isPerfectShallow b) 
--                  (reverse $ sort as'')
--  where 
--    as' = filter (\(a,_) -> hasContent a) as -- remove function-only stuff
--      where hasContent (A (t,u)) = (not . null) (contentTags t) 
--                                || (not . null) (contentTags u)
--    as'' = case mmax of
--      (Just m) -> 
--        filter (\(A (t,u),_) -> sizeRTree t <= m || sizeRTree u <= m) as'
--      Nothing -> as'

-- | Check if an alignment is perfect, i.e. ig the structure of the two trees
-- is the same
--isPerfect :: Alignment -> Bool
--isPerfect (A (t,u)) = abstractUDTree t == abstractUDTree u 

-- | Check the top-level structure of the two trees composing an alignment
-- is the same
--isPerfectShallow :: Alignment -> Bool
--isPerfectShallow (A (t,u)) 
--  = root t' == root u' 
--  && map root (childrenRTree t') == map root (childrenRTree u') 
--  where (t',u') = (abstractUDTree t,abstractUDTree u)

-- | Return the id of a sentence, taken from the comment that precedes it
-- assumption: the CoNLL-U files are generated by ExtractConcepts and
-- PropagateConcepts and have not been modified
sentId :: UDSentence -> Int -- TODO: mv (to gf-ud), make more robust
sentId = read . drop 4 . last . words . head . udCommentLines