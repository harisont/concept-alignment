# Potential paper structure

### Introduction
Similar to the thesis' introduction but emphasizing the "construct lexica for CNLs" aspect. 

### Related works
- other alignment techniques (phrase & word alignment)
- technologies the program is built on top of 
  > skipping the parts introducing constituency and dependency grammars
  - GF
  - UD
  - `gf-ud`

### Methodology
- definitions
  - concept
  - alignment
  - CE & CP (focus on CE)
- overview 
  > basically pag 17 (diagram)
- CE algorithm
  > without making distinctions between the baseline and what was done in the context of the thesis project; pseudocode instead of Haskell simplified code? 
- discussion of the various refinements
  - alignment criteria
    > mentioning details such as divergences only briefly
  - pruning (?)
  - heads
  - gf-ud patterns
  - ...
- CP algorithm (?) 
  > treated as preliminary or even omitted entirely
- GG (lexicon generation)

### Evaluation
- data 
  > same as in the thesis
  - PUD
  - course plans
- metrics
- experiments
  > basically part of those presented in the thesis; it might or might not make sense to run them again 
  - manual: en-sv vs en-it (?)
  - comparison with `fast_align`
  - MT experiments

### Conclusions
- what software was developed (and where it can be found)
  > here it might make more sense to mention CP as something ongoing
- summary and discussion of results
- future work (not necessarily in this order)
  - hybrid system using statistical techniques too (?)
  - CP optimization and/or n-lingual CE
    > hopefully this will be more like "current work"
  - potential lang-specific extensions (?)
  - related tools, e.g. interactive GG, conllu viewer etc. (?)
  - sentence alignment (to be integrated or developed) )?)
