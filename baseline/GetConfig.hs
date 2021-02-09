module GetConfig where

import Utils 
import TreeConv
import Data.List (intersperse,nub,isPrefixOf,isSuffixOf)
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import qualified Data.Set as S 
import qualified PGF
import GF (BindType(Explicit)) ---- should not need to import this!!
import Text.PrettyPrint (render)

getConfiguration :: [FilePath] -> Maybe PGF.PGF -> IO Configuration
getConfiguration files mpgf = do
  ss <- mapM readFile files >>= return . concatMap lines
  let ls  = [takeWhile (/="--") (words l) |                                    -- possible tail comment separated by --
                   l@(c:_) <- ss, notElem c "-"]                               -- non-empty lines, not starting with -
  let metalines = [ws | ws@(_:_) <- ls, elem "#" ws]
  let catlines  = [ws | ws@(_:_) <- ls, not (any (flip elem ws) [";", ":", "=", "#"]) ]
  let funlines  = [ws | ws@(w:_) <- ls, w /="*",elem ":" ws]                   -- functions with type-sigs in the config file
  let backuplines = [ws | "*":ws <- ls]
  let deflines  = [ws | ws@(w:_) <- ls, elem "=" ws, w /= "cat", w /= "lbl"]
  let helpcatlines = [(k,c) | "cat":k:"=":c:_ <- ls]

  let helplablines = [(k,[c]) | "lbl":k:"=":cs <- ls, c <- cs]                 -- macros used for labels
  let labmacros = M.fromListWith (++) helplablines 
  let extfunlines = [ews | ws <- funlines, ews <- replaceAll labmacros ws] 

  let lablines  = [ws | ws@(w:_) <- ls, w /="*",elem ";" ws, notElem ":" ws]   -- labels for pgf functions without type-sigs
  let extlablines = [ews | ws <- lablines, ews <- replaceAll labmacros ws] 
  let labmap    = M.fromList [(f,(ls,ms)) | f:";":lms <- extlablines, let (ls,ms) = break (elem '=') lms]
  
  let pgfmap    = maybe M.empty (M.fromList . pgfFunctions) mpgf
  let pgffuns   = [initFunInfo {funid = f,
                                valtype = c,
                                argtypes = zipCatsLabels cs ls,
                                morphoconstraints = map pMorConstraint ms      -- morph. constraints
                                }
                        |
                      (f,(c,cs)) <- M.assocs pgfmap,
                      Just (ls,ms) <- [M.lookup f labmap]
                  ]
  let newfuns   = [fi |
                     fi <- map pFunInfo extfunlines,
                     case M.lookup (funid fi) pgfmap of
                       Just (c,cs) | valtype fi /= c || map fst (argtypes fi) /= cs  ->
                          error $ "CONFIGURATION ERROR: wrong type given to " ++ funid fi
                       _ -> True
                  ]
  let allfuns = newfuns ++ pgffuns 
  let bups = map pFunInfo backuplines
  let helps = M.fromList helpcatlines
  
  return $ Conf {
    grammarname = case metalines of {(_:name:_):_ -> name ; _ -> error "cannot find grammar name"},
    categories  = pCatMap catlines,
    functions   = allfuns,
    backups     = bups,
    definitions = getDefMap mpgf allfuns helps deflines,
    helpcategories = helps
    }

pMorConstraint :: String -> LMorphoConstraint 
pMorConstraint mc = case break (=='=') mc of
  (lf, '=':v)  -> case break (=='.') lf of 
                   (l, '.':f) -> (l,      (f, v))
                   (f, "")    -> ("head", (f, v))
  (_,  "")      -> ("head", (mc, ""))     --- error $ "ERROR: no morph info from " ++ mc

pFunInfo :: [String] -> FunInfo
pFunInfo s = case s of
  f : ":"  : ts ->
    let
      (typs,_:labsmor) = break (==";") (filter (/="->") ts)
      (labs,mor) = break (elem '=') labsmor
      args = zipCatsLabels (init typs) labs
    in initFunInfo {
      funid = f,
      valtype = (last typs),
      argtypes = args,
      morphoconstraints = map pMorConstraint mor
      } 
  _ -> error $ "ERROR: no function info from " ++ unwords s

zipCatsLabels typs labs = if (elem (length typs) [1,length labs])
  then zip typs (labs ++ ["head"])                                             -- head not marked for 1-arg functions
  else error $ "ERROR: unmatched arguments and labels in " ++ unwords typs ++ " ; " ++ unwords labs

pCatMap :: [[String]] -> CatMap
pCatMap = M.fromListWith (++) . concatMap pCatInfo where
  pCatInfo s = case s of
     c:pms -> [(p, [initCatInfo {
                       catid=c, 
                       mconstraints=map pMorConstraint ms
                    } ] ) | let (ps,ms) = break (elem '=') pms, p <- ps ]
     _ -> error $ "ERROR: no category info from " ++ unwords s

getDefMap :: Maybe PGF.PGF -> [FunInfo] -> M.Map Cat Cat -> [[String]] -> DefMap
getDefMap mpgf funs helps = M.fromList . filter check . map pDefInfo where
  pDefInfo s = case break (=="=") s of
     (p:vs, _:c:cs) -> (p, (vs, pGFTree (unwords (c:cs))))
     _ -> error $ "ERROR: no definition (yet) from " ++ unwords s
  check d@(p,(vs,t)) =
    let
      typ = case lookup p [(funid fi, (valtype fi, map fst (argtypes fi))) | fi <- funs] of
          Just (c0,cs0) ->
             let (c:cs) = map normalizeCat (c0:cs0) in
             PGF.mkType [PGF.mkHypo (PGF.mkType [] (PGF.mkCId x) []) | x <- cs] (PGF.mkCId c) []
          Nothing -> error $ "ERROR IN DEFINITION: unknown function " ++ p
      exp = mkExp vs t
    in case mpgf of
      Nothing -> True
      _ | isPrefixOf "Backup" p -> True   ---- to allow temporary backup functions; TODO these should disappear
      Just pgf -> either (error . (("ERROR IN DEFINITION OF " ++ p ++ " : ")++) . render . PGF.ppTcError)
                         (const True) (PGF.checkExpr pgf exp typ)
  mkExp vs (T f ts) = foldr (PGF.mkAbs Explicit)   -- \vs -> f ts ; --- the PGF.mkAbs API is very low-level
                         (mkFunApp (fun f) (map (mkExp []) ts)) [PGF.mkCId v | v <- vs]
  mkFunApp s = case s of
      '"':_:_ | last s == '"' -> const (PGF.mkStr s)
      _ -> PGF.mkApp (PGF.mkCId s)

  normalizeCat c = maybe c id $ M.lookup c helps


pGFTree :: String -> GFTree
pGFTree s = case PGF.readExpr s of
  Just e -> mkGF e
  _ -> error $ "ERROR: cannot parse GF tree " ++ s
 where
   mkGF e = case PGF.unApp e of
     Just (f,es) -> apps (PGF.showCId f) (map mkGF es)
     _ -> error $ "ERROR: cannot build GF tree from " ++ s

pgfFunctions :: PGF.PGF -> [(Fun,(Cat,[Cat]))]
pgfFunctions pgf = [
    (PGF.showCId f, typ) |
      f <- PGF.functions pgf, 
      Just ft <- [PGF.functionType pgf f],
      let (hs,cid,_) = PGF.unType ft, not (null hs),                           -- ft must be a true function type
      let typ = (PGF.showCId cid,
            [PGF.showCId c | (_,_,vt) <- hs, (_,c,_) <- [PGF.unType vt]])
      ]

-- from Conll2tagged.hs
-- reading stanzas for each sentence from a conll file
getSentences ls = case dropWhile (isJunk) ls of
    [] -> []
    ls2 -> case break isJunk ls2 of
      (ls21,[]) -> [ls21]
      (ls21,ls22) -> ls21 : getSentences ls22
  where isJunk line = all isSpace line || take 1 line == "#"

-- from Analyse.hs
fromTabs = map (getSep '\t') . lines
toTabs = unlines . map (blocks2sep "\t")

getSep :: Eq a => a -> [a] -> [[a]]
getSep sep xs = case break (==sep) xs of
  (x1,_:x2) -> x1 : getSep sep x2
  (x1,[]) -> x1 : []

blocks2sep :: String -> [String] -> String
blocks2sep sep = concat . intersperse sep

-- NEW: analyze configurations with respect to a PGF and see how many functions
-- are covered and how many are not covered by the configurations
-- this helps make sure that all functions in the grammar are either covered
-- using configurations or intentionally left out to escape undesired affects
missingConfigs :: PGF.PGF -> Dictionary -> Configuration -> [(Cat,[Fun])]
missingConfigs gram dict conf = [(c, [f|(c_,f) <- allfuns, c==c_, notElem f cfgfuns]) | c <- allcats] 
  where
    -- get MWE functions from dictionary rather than PGF
    mwefuns = [(cat, fun)  | ((w,cat), funs)  <- M.toList dict, length (words w) > 1, fun<-funs]
    pgffuns = [(cat, fun)  | (fun,(cat,args)) <- pgfFunctions gram, not $ isPrefixOf "Backup" fun]
    allfuns = mwefuns ++ pgffuns
    allcats = [PGF.showCId c | c <- PGF.categories gram]
    cfgfuns = [funid fun | fun <- (functions conf)] 
                ++ [funid fun | fun <- (backups conf)] 
                ++ nub [fun node | (vars,exp) <- M.elems (definitions conf), node <- allnodes exp, notElem (fun node) vars] 

-- dictionary from PGF
-- problems: first lin not always the lemma; cannot get variants
pgfDictionary :: Configuration -> PGF.PGF -> Lang -> Dictionary
pgfDictionary config pgf lang = M.fromListWith (++) (configlex ++ rules) where
  rules = [((w, c), [PGF.showCId f]) |                                         -- (lemma,category) -> function
              (k,c) <- [(k,c) |
                 k <- PGF.categories pgf, let c = PGF.showCId k,               -- consider only categories present in pgf
                 elem c cats],                                                 -- consider only categories used in config
              f <- PGF.functionsByCat pgf k,                                   -- take any function from such a category
              Just ft <- [PGF.functionType pgf f],                             -- inspect the type of the function
              let (hs,cid,_) = PGF.unType ft, null hs,                         -- check that it is a basic type (i.e. no args)
              let w0 = PGF.linearize pgf cncname (PGF.mkApp f []),             -- take the first lin ---- assumes this is the lemma
              let w = unlexBind w0,                                            -- compute bind tokens (needed in stemmed Finnish)
              length (words w) == 1                                            -- exclude multiwords
              ]
  cats = nub $ map catid $ concat $ M.elems $ categories config
  cncname_ = PGF.mkCId (grammarname config ++ lang)                             -- e.g. UDTranslate ++ Swe  (doesn't work with mono.grammar)
  cncname = PGF.mkCId $ head (filter (isSuffixOf lang) (map PGF.showCId (PGF.languages pgf)))  -- works for all grammars?
  configlex = [((w,catid ci),[quote w]) |                                      -- add strings from config to the lexicon
                (p,cis) <- M.assocs (categories config), ci <- cis,            -- they come from category definitions of form
                ("head",("lemma",w)) <- mconstraints ci                        --    <cat> <pos> lemma=<word>
              ] ++                                                             -- e.g.  Cop_ VERB lemma=be -> ((be,Cop_),"be")
              [((w, valtype fi),[funid fi]) | 
                fi <- functions config, null (argtypes fi),                    -- they can also come from 0-place function definitions
                ("head",("lemma",w)) <- morphoconstraints fi                   -- of form <fun> : <cat> ; lemma=word
              ]                                                                -- e.g.  must_VV : VV ; lemma=must
 
-------------------
-- to be deprecated (or made permanent :) )
---- can be used for Fin and Eng now; more languages to be added
---- should be avoided: use pgfDictionary instead. Subcategories don't get right here.
-- TODO: modify this to accept tsv format for arbitrary #languages 
tsvDictionary :: Configuration -> [FilePath] -> Lang -> IO Dictionary
tsvDictionary config files lang = do
  ds <- mapM readFile files >>= return . concatMap fromTabs
  let configlex  = [((w, catid ci),[quote w]) | 
                     (p,cis) <- M.assocs (categories config), ci <- cis,
                     ("head",("lemma",w)) <- mconstraints ci
                   ]       -- import syncategorematic words only from category definitions
                           -- 0-place function definitions are ignored
                           -- because they can conflict with entries in the tsv file
  let lexentries = [((w,c), words f) | d@(c:_id:fw:ew:_comment:f:_) <- ds,          -- format from fiwn
                                       let w = if lang == "Fin" then fw else ew] 
  case lang of 
    _ | elem lang ["Fin","Eng"] -> return $ M.fromListWith (++) (configlex ++ lexentries)
    _                           -> error $ "only languages Eng and Fin recognized in this dictionary"

{-
    - variant dictionary 
N    albatross   albatross_N albatross_1_N albatross_2_N
-}

tsvLexicon :: Configuration -> [FilePath] -> IO Dictionary
tsvLexicon config files = do
  ds <- mapM readFile files >>= return . concatMap fromTabs
  let configlex  = [((w, catid ci), [quote w]) | 
                     (p,cis) <- M.assocs (categories config), ci <- cis,
                     ("head",("lemma", w)) <- mconstraints ci
                   ]       -- import syncategorematic words only from category definitions
                           -- 0-place function definitions are ignored
                           -- because they can conflict with entries in the tsv file
  let lexentries = [((ew,c), words f) | d@(c:ew:f:_) <- ds]
  return $ M.fromListWith (++) (configlex ++ lexentries)

{-
-- Dictionary mapping from data

lin thing_N = mkN "thing" "things";

*Analyse> ds <- readFile "/Users/aarne/GF/lib/src/translator/DictionaryEng.gf" >>= return . lines
*Analyse> let dmap = M.fromListWith (++) [(multiwordOfFun v,[v]) | "lin":v:_ <- map words ds, catOfFun v == "N"]

N	n00002452	esine	thing

*Analyse> ws <- readFile "nouns.tmp" >>= return . fromTabs
*Analyse> writeFile "fin-eng-nouns.tsv" $ toTabs [v ++ [unwords fs] | v@(_:_:_:e:_) <- ws, Just fs <- [M.lookup e dmap]]

N	n00002452	esine	thing		thing_N


catOfFun = reverse . takeWhile (/='_') . reverse ---- the GF Dict convention...
-}

-- if external lexicon provides functions not included in the grammar
-- without reducing the size of the lexicon
-- remove functions that are not included in the grammar
cleanDictionary ::  S.Set PGF.CId -> Dictionary -> Dictionary
cleanDictionary funs dict = M.fromList [(lem, case ffuns of {[] -> lfuns ; _ -> ffuns}) | (lem,lfuns) <- M.toList dict, let ffuns=[f|f<-lfuns,elem (PGF.mkCId f) funs] ]

