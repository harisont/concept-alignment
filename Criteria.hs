module Criteria (criteria) where

import qualified Data.Set as S
import RTree
import UDConcepts
import ConceptAlignment

-- Modify this modules to add/remove/change alignments criteria.
-- NOTE: this module will only export the list of criteria below! 

-- | List of criteria used by align, sorted by priority
criteria :: [Criterion]
criteria = [udpos, ud, divs, pass, pos]

{- Functions used in criteria -}

-- | Original basic criterion: matching root UD labels 
udMatch :: UDTree -> UDTree -> Bool
(RTree n ts) `udMatch` (RTree m us) = udSimpleDEPREL n == udSimpleDEPREL m

-- | POS-equivalence
posEquiv :: UDTree -> UDTree -> Bool
t1 `posEquiv` t2 = (not . null) ct1 && (ct1 == ct2)
  where (ct1, ct2) = (contentTags t1, contentTags t2)
  
-- | A an adverb is translated as a PP (structural divergence)
advmodObl :: UDTree -> UDTree -> Bool
advmodObl t u = t `isLabelled` "advmod" && u `isLabelled` "obl"
                -- POS-equiv would be too strict here
                && length (contentTags t) == length (contentTags u)

-- | An adjective is translated as a nmod (categorial divergence)
amodNmod :: UDTree -> UDTree -> Bool
amodNmod t u = t `isLabelled` "amod" && u `isLabelled` "nmod"
                && subtreesTags t == subtreesTags u 

-- | An adjective is translated as an adverb (categorial divergence)
amodAdvmod :: UDTree -> UDTree -> Bool
amodAdvmod t u = t `isLabelled` "amod" && u `isLabelled` "advmod"
                && subtreesTags t == subtreesTags u

-- | A verb is transitive in the SL but not in the TL
-- (structural divergences regarding object)
objObl :: UDTree -> UDTree -> Bool
objObl t u = t `isLabelled` "obj" && u `isLabelled` "obl"
             && t `posEquiv` u

-- | The indirect object of a verb in the SL is rendered as a PP in the TL
-- (structural divergences regarding indirect object)
iobjObl :: UDTree -> UDTree -> Bool
iobjObl t u = t `isLabelled` "iobj" && u `isLabelled` "obl"
              && t `posEquiv` u

-- | The indirect object of a sentence is the object in its translation
iobjObj :: UDTree -> UDTree -> Bool
iobjObj t u = t `isLabelled` "iobj" && u `isLabelled` "obj"
             && t `posEquiv` u

-- | A verb is transitive in the SL but not in the TL
-- (structural divergences regarding subject)
nsubjObl :: UDTree -> UDTree -> Bool
nsubjObl t u = t `isLabelled` "nsubj" && u `isLabelled` "obl"
             && t `posEquiv` u

-- | The sentence in the TL is passive and its translation is active:
-- passive subject becomes object
passSubjObj :: UDTree -> UDTree -> Bool
-- t's label is checked subtypes included, so isLabelled can't be used
passSubjObj t u = udDEPREL (root t) == "nsubj:pass" && u `isLabelled` "obj"
              && t `posEquiv` u

-- | The sentence in the TL is passive and its translation is active:
-- agent becomes subject
passOblSubj :: UDTree -> UDTree -> Bool
passOblSubj t u = t `isLabelled` "obl" && u `isLabelled` "nsubj"
                  && t `posEquiv` u

{- Some language pair independent-ish criteria -}

ud, pos, divs, udpos, pass :: Criterion
ud = C udMatch (S.singleton UD) True False
pos = C posEquiv (S.singleton POS) True False 
udpos = 
  C (\t u -> udMatch t u && posEquiv t u) (S.fromList [UD,POS]) True True
divs = C (\t u -> 
  or [
    advmodObl t u, advmodObl u t, 
    objObl t u, objObl u t,
    iobjObj t u, iobjObj u t,
    nsubjObl t u, nsubjObl u t,
    --iobjObl t u, iobjObl u t,
    amodNmod t u, amodNmod u t,
    amodAdvmod t u, amodAdvmod u t
  ]) (S.singleton DIV) False False
pass = C (\t u -> or [passSubjObj t u, passSubjObj u t,
                      passOblSubj t u, passOblSubj u t]) 
          (S.fromList [PASS, DIV]) True False