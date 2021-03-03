module ConceptAlignment where

import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.MultiSet as MS
import qualified Data.Map as M
import qualified Data.Set as S
import RTree
import UDConcepts
import UDPatterns

{- Basic (mostly data types) definitions and instances for alignments etc. -}

-- | An alignment is a pair of corresponding UD subtrees (not a type synonym
-- because of custom Eq and Ord instances)
-- TODO: type synonym instances and just a pair?
newtype Alignment = A (UDTree,UDTree)

-- | Left (SL) tree of an alignment
sl :: Alignment -> UDTree
sl (A (t,u)) = t

-- | Correct (TL) tree of an alignment
tl :: Alignment -> UDTree
tl (A (t,u)) = u

-- | Alignments are shown as pairs
instance Show Alignment where
  show (A a) = show a

-- | prAlignment is used instead of show to "visually" inspect the alignments
prAlignment :: Alignment -> String
prAlignment a = t' ++ "|" ++ u'
  where (t',u') = linearize a

-- | Two alignment are equal whenever their "linearizations" are the same
instance Eq Alignment where
  a == b = linearize a == linearize b

-- | Linearize the alignment to a pair of strings, ignoring initial whitespace
linearize :: Alignment -> (String,String)
linearize (A (t,u)) = (prUDTreeString' t,prUDTreeString' u)
  where prUDTreeString' = dropWhile (== ' ') . prUDTreeString

-- | Alignments can be ordered based on:
-- 1. size of their left tree
-- 2. size of their Correct tree
-- 3. "linearization" of their left tree (alphabetically)
-- 4. "linearization" of their Correct tree (alphabetically)
instance Ord Alignment where
  (A (t1,u1)) <= (A (t2,u2)) = k t2 u2 <= k t1 u1
    where 
      k t u = (depthRTree t,depthRTree u,prUDTreeString t,prUDTreeString u)

-- | Check if an alignment contains another;
-- used both in pruning and selection of alignments for MT
contains :: Alignment -> Alignment -> Bool
(A (t1,u1)) `contains` (A (t2,u2)) = t2 `isSubRTree` t1 && u2 `isSubRTree` u1

-- | Alignments is a mapping where keys are of type Alignment and values are 
-- tuples with other relevant info 
type Alignments = M.Map Alignment Info

-- | Relevant infos are the set of reasons for an alignment and its number of
-- occurrences
type Info = (S.Set Reason,Int)

-- | Record type for linearized alignments, mostly used in EvAlign
data LinAlignment = LAlignment {
  ltrees :: (String, String),
  lreasons :: [Reason],
  loccurrences :: Int
} deriving (Eq)

-- | Custom instance of Show for LinAlignments
instance Show LinAlignment where
  show (LAlignment (t,u) r n) = t ++ "|" ++ u ++ show r ++ show n

-- | Custom instance of Read for LinAlignments
instance Read LinAlignment where
  readsPrec _ s = [(LAlignment (t,u) r n, "")]
    where 
      [t,u] = splitOn "|" $ takeWhile (/='[') s
      r = 
        (read $ takeWhile (/=']') (dropWhile (/='[') s) ++ "]") :: [Reason]
      n = read (tail $ dropWhile (/=']') s) :: Int

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

-- | Alignment can be associated with annotations
type AnnotatedAlignment = (Annotation,LinAlignment)

-- | Convert an alignment and its "statistics" to a LinAlignment
toLinAlignment :: (Alignment, (S.Set Reason, Int)) -> LinAlignment
toLinAlignment (a, (rs, n)) = 
  LAlignment (linearize a) (S.toList rs) n

-- | Combine reasons and n_occurrences 
-- (used for union(s)With, fromListWith etc.) 
combineVals :: Info -> Info -> Info
combineVals (r,n) (s,m) = (r `S.union` s,n + m)

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
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

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
-- (that's why it isn't defined in module Criteria)
cl :: Criterion
cl = C (\t u -> divClause t u || divClause u t) (S.singleton CL) True False
  where 
    -- a clause of a certain type is translated as a clause of another type 
    divClause :: UDTree -> UDTree -> Bool
    divClause t u = or [t `isLabelled` r | r <- clDEPRELs] 
                 && or [u `isLabelled` r | r <- clDEPRELs]

{- Alignment functions -}

-- | Align a list of pairs of / corresponding / dependency trees   
align :: [LinAlignment]       -- ^ a list of known linearized alignments
      -> [Criterion]          -- ^ a list of criteria (sorted by priority)
      -> Maybe UDReplacement  -- ^ a replacement pattern
      -> Bool                 -- ^ a flag indicating whether clause 
                              -- segmentation should be performed  
      -> Bool                 -- ^ a flag indicating whether alignment "by 
                              -- exclusion" should also be performed 
      -> [(UDTree,UDTree)]    -- ^ the list of pairs of dependency trees
      -> Alignments           -- ^ a Map of alignments
align = align' M.empty
  where
    align' as _ _ _ _ _ [] = as
    align' as las cs r segment byExcl (tu:tus) =
      align' (
        M.unionWith combineVals as (
          alignSent as las cs r segment byExcl tu
        )
      ) las cs r segment byExcl tus

-- | Sentence-level alignment function
alignSent :: Alignments           -- ^ a map of known alignments
          -> [LinAlignment]       -- ^ a list of known linearized alignments
          -> [Criterion]          -- ^ a list of criteria (sorted by priority)
          -> Maybe UDReplacement  -- ^ a replacement pattern
          -> Bool                 -- ^ a flag indicating whether clause 
                                  -- segmentation should be performed 
          -> Bool                 -- ^ a flag indicating whether alignment "by 
                                  -- exclusion" should also be performed 
          -> (UDTree,UDTree)      -- ^ a pair of dependency trees 
          -> Alignments           -- ^ a Map of alignments
alignSent as las cs r segment byExcl (t,u) = 
  M.fromListWith combineVals (as' ++ as'')
  where
    -- "basic" alignment based on sentence/clause recursive alignment
    as' = if segment then alignClauses as cs (t,u) else alignSent' as cs (t,u)
    -- attempts to align subtrees that could not be aligned properly
    as'' = if byExcl 
      then alignRest (M.fromListWith combineVals as') cs (t,u)
      else []
    
    -- actual recursive function working with a list of (key,val) pairs 
    alignSent' as cs (t@(RTree n ts), u@(RTree m us))
      -- 1+ criteria match
      | (not . null) cs' = prune as'
      | A (t,u) `M.member` as = [
          (A (t,u),(S.singleton PREV,1)), 
          (alignHeads (A (t,u)),(S.fromList [HEAD,PREV], 1))
        ]
      | linearize (A (t,u)) `elem` map ltrees las = [
          (A (t,u),(S.singleton FAST,1))
        ]
      | otherwise = []
        where
          -- applying criteria 
          cs' = map snd (filter fst (zip [f t u | f <- map func cs] cs))
          c = head cs' -- the first determines reasons & if heads are aligned
          -- new alignments, subtrees included
          as' = case (headAlign c,isJust mtup) of
            (True,True) -> h:tu:tup:sas
            (True,False) -> h:tu:sas
            (False,True) -> tu:tup:sas
            (False,False) -> tu:sas
            where 
              tu = (A (t,u),(reas c,1)) -- (t,u)
              mtup = if isJust r 
                then alignPattern (fromJust r) (fst tu) 
                else Nothing
              tup = (fromJust mtup,(S.insert PM (reas c), 1))
              h = (alignHeads $ fst tu,(S.insert HEAD (reas c), 1)) -- heads 
              -- subtree alignments
              sas = concatMap (alignSent' as cs) [(t,u) | t <- ts', u <- us']
                where (ts',us') = (sortByLabel ts,sortByLabel us)
                        where sortByLabel = sortOn (udSimpleDEPREL . root)
    
    -- call alignSent' on all pairs of clauses found in t and u
    alignClauses as cs (t,u) = 
        prune $ concatMap (alignSent' as (cs ++ [cl])) clausePairs
      where 
        clausePairs = [(ct,cu) | ct <- cts, cu <- cus, 
                                 length (clauses ct) == length (clauses cu)]
        -- get all clauses, sorted by dep. relation and distance from root
        (cts,cus) = (sortCs $ clauses t, sortCs $ clauses u)
        sortCs = sortOn (\(RTree n ts) -> 
          (udSimpleDEPREL n,dependencyDistance n))

    -- call alignSent' on all pairs of unaligned nominals + modifiers
    -- ("alignment by exclusion")
    alignRest as cs (t,u) = 
      prune $ concatMap (alignSent' as cs') nomPairs
        where
          nomPairs = [(nt,nu) | nt <- nts, nt `notElem` las, 
                                nu <- nus, nu `notElem` ras,
                                length (nommods nt) == length (nommods nu)]
          -- get all nominals and modifiers, sorted by label 
          -- and distance from root
          (nts,nus) = (sortNs $ nommods t, sortNs $ nommods u)
          sortNs = sortOn (\(RTree n ts) -> 
            (udSimpleDEPREL n,dependencyDistance n))
          (las,ras) = unzip $ map (\(A x) -> x) (M.keys as)
          cs' = map 
            (\(C f _ h s) -> C f (S.singleton REST) h s) 
            (filter strict cs)

-- | Helper function removing the less valid alternative alignments
prune :: [(Alignment,Info)] -> [(Alignment,Info)]
-- two different sorting functions are used one after another so it's easier
-- to change the code to une only one of them or change the order in which
-- they are applied
prune = nubBy areAlt . sortByFertility . sortByReasons
  where 
    -- check if two alignments are alternative to each other comparing THE
    -- ACTUAL TREES, and not their "linearizations"
    areAlt (A (t1,u1),_) (A (t2,u2),_) = t1 == t2 || u1 == u2
    -- sort alignments by number of reasons (decreasing order), then by first
    sortByReasons = sortOn (\(_,(rs,n)) -> 
      let rs' = rs `S.difference` S.fromList [HEAD,PREV,PM] 
      in (-(length rs'), maximum $ S.elems rs))
    -- alt. fertility-based sorting strategy
    -- top alignments are the ones that lead to more sub-alignments
    sortByFertility as = sortOn (\(a,_) -> - (length $ subas a)) as
      where subas a = filter (\a' -> a `contains` a') (map fst as)

-- | Helper function for pattern alignment: given an alignment and a 
-- replacement pattern, return the corresponding pattern-replaced one 
-- (if applicable)
alignPattern :: UDReplacement -> Alignment -> Maybe Alignment
alignPattern r (A (t,u)) = 
  case (ch1,ch2) of
    -- only return an alignment if the pattern applies to both members
    (True,True) -> Just (A (t',u'))
    _ -> Nothing
  where 
    (t',ch1) = replacementsWithUDPattern r t
    (u',ch2) = replacementsWithUDPattern r u

-- | Helper function for head alignment: given an alignment, return a new one  
-- for their "heads", respecting any compounds and aux+verbs (and more?)
alignHeads :: Alignment -> Alignment
alignHeads (A (RTree n ts,RTree m us))
  -- if there are compound constructions, look for their counterparts and
  -- align accordingly
  | (not . null) cts = A (RTree n cts, RTree m (compCounterparts ts us))
  | (not . null) cus = A (RTree n (compCounterparts us ts), RTree m cus)
  -- if the roots are verbs (to avoid messing with copulas) and only one of
  -- them has 1+ auxiliaries, align verb | verb + auxiliaries
  | all isVerb [n,m] && (not . null) ats && null aus = 
      A (RTree n ats, RTree m [])
  | all isVerb [n,m] && null ats && (not . null) aus = 
      A (RTree n [], RTree m aus) 
  | otherwise = A (RTree n [], RTree m [])
  where
    -- select subtrees labelled in a certain way 
    filterByLabel l xs = filter (`isLabelled` l) xs

    ats = filterByLabel "aux" ts
    aus = filterByLabel "aux" us
    cts = filterByLabel "compound" ts
    cus = filterByLabel "compound" us

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
propagate :: [Criterion]            -- ^ a list of criteria 
                                    -- (sorted by priority)
          -> Bool                   -- ^ -- ^ a flag indicating whether clause 
                                    -- segmentation should be performed
          -> Bool                   -- ^ a flag indicating whether alignment
                                    --"by exclusion" should also be performed 
          -> ([UDTree],[UDTree])    -- a pair of lists of UD trees (the
                                    -- sentences to propagate on) in L1, L2
          -> UDTree                 -- a previously extracted L1 concept
          -> Maybe (Alignment,Info) -- an (Alignment,Info) pair, if found
propagate cs segment byExcl ([],_) _ = Nothing 
propagate cs segment byExcl (t:ts,u:us) c =
  let 
    as = M.toList $ alignSent M.empty [] cs Nothing segment byExcl (t,u)
    as' = case (c `isSubUDTree'` t, c `isHeadSubUDTree` t) of
      (True,_) -> sortOnDepth as
      (_,True) -> sortOnDepth (filter (\(_,(rs,_)) -> HEAD `elem` rs) as)
      (False,False) -> []
    in case find (\(a,_) -> c =~ sl a) as' of
      Nothing -> propagate cs segment byExcl (ts,us) c
      n -> n
    where 
      -- difference between the depth of the SL aligned subtree and c:
      -- the smaller, the better (mostly used to avoid that root nodes are
      -- aligned with full sentences, but also makes sense in general)
      depthDiff :: RTree a -> Int
      depthDiff t = abs (depthRTree t - depthRTree c)
      sortOnDepth :: [(Alignment,Info)] ->[(Alignment,Info)]
      sortOnDepth = sortOn (depthDiff . sl . fst)
      -- check if c is the head of one of t's subtrees
      isHeadSubUDTree :: UDTree -> UDTree -> Bool
      isHeadSubUDTree c t = 
        isJust $ listToMaybe $ 
          sortOn depthDiff (filter (isHeadUDTree c) (allSubRTrees t))

-- check if a UD tree is the head of another
isHeadUDTree :: UDTree -> UDTree -> Bool
isHeadUDTree (RTree n []) (RTree m _) = n =~ m
isHeadUDTree (RTree n ts) (RTree m _) = n =~ m && (hasAuxOnly || hasCompOnly) 
  where 
    hasAuxOnly = all (`isLabelled` "aux") ts
    hasCompOnly = all 
      (\t -> 
        udSimpleDEPREL (root t) `elem` ["compound", "flat", "nmod", "amod"]
      ) 
      ts

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
alignment2sentencePair (A (t,u)) = (subtree2Sentence t, subtree2Sentence u)

-- | Convert a subtree into a "complete" UD tree (has a root, nodes [1..n]) in 
-- valid CoNLL-U format
subtree2Sentence :: UDTree -> UDSentence
subtree2Sentence = adjustIds . markRoot
  where 
    markRoot (RTree n ts) = RTree (n { 
      udDEPREL = "root:" ++ udDEPREL n,
      udHEAD = udIdRoot
    }) ts
    adjustIds t = UDSentence cs (map (replaceIds ids) us)
      where 
        (UDSentence cs us) = udTree2sentence t
        ids = map udID us
        replaceIds ids w = case udHEAD w of 
          udIdRoot -> w {
            udID = i' $ udID w
          }
          udIDInt -> w {
            udID = i' $ udID w,
            udHEAD = i' $ udHEAD w
          }
          where i' i = UDIdInt $ 1 + fromJust (i `elemIndex` ids)


{- Selection of alignments for MT -}

-- | Select the alignments relevant for MT. Namely:
-- - remove anything that does not contain any content word
-- - remove redundant alignments (i.e. alignments that can be inferred
--   from their sub-alignments)
-- - optionally, remove alignments where the size of both trees is > n
selectForMT :: Maybe Int -> [(Alignment,Info)] -> [(Alignment,Info)]
selectForMT mmax as = nubBy 
                  (\(a,_) (b,_) -> b `contains` a && isPerfectShallow b) 
                  (reverse $ sort as'')
  where 
    as' = filter (\(a,_) -> hasContent a) as -- remove function-only stuff
      where hasContent (A (t,u)) = (not . null) (contentTags t) 
                                || (not . null) (contentTags u)
    as'' = case mmax of
      (Just m) -> 
        filter (\(A (t,u),_) -> sizeRTree t <= m || sizeRTree u <= m) as'
      Nothing -> as'

-- | Check if an alignment is perfect, i.e. ig the structure of the two trees
-- is the same
isPerfect :: Alignment -> Bool
isPerfect (A (t,u)) = abstractUDTree t == abstractUDTree u 

-- | Check the top-level structure of the two trees composing an alignment
-- is the same
isPerfectShallow :: Alignment -> Bool
isPerfectShallow (A (t,u)) 
  = root t' == root u' 
  && map root (childrenRTree t') == map root (childrenRTree u') 
  where (t',u') = (abstractUDTree t,abstractUDTree u)