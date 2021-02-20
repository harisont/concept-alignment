---
title: Syntax-based Concept Alignment for Machine Translation
subtitle: Master's thesis, A.Y. 2020-2021
author: Arianna Masciolini
theme: mhthm
---

# Concept Alignment

## A first definition
__Concept Alignment__: the task of finding semantical correspondences between parts of multilingual parallel texts.

![From Lewis Carroll, _Alice's adventures in Wonderland_. Parallel text at `paralleltext.io`](figures/alice_sentence.png)

## CA at different levels of abstraction
Word alignment:
![](figures/alice_word.png)

\pause

Phrase alignment: 
![](figures/alice_phrase.png)


## Subtasks
> - __Concept Extraction__: identifying new concepts via linguistic comparison
> - __Concept Propagation__: finding expressions corresponding to known concepts in a particular language

## CA in translation
A human translator \pause

1. recognizes concepts in the text to translate
2. looks for ways to render them in the target language

\pause
...same idea behind _compositional_ Machine Translation.

## Semantic compositionality
The meaning of a complex expression is determined by:

- the meanings of its components (lexical semantics)
- the way its components are combined with each other (syntax)

\pause
The _translation_ of a complex expression is given by:

- the _translations_ of its components (lexical semantics)
- the way its components are combined with each other (syntax, taking cross-lingual divergences into account)

## Statistical approaches
Standard approaches to automation are statistical (IBM models)

Issues: \pause

> - "fixed" level of abstraction (generally either word or phrase alignment)
> - correspondences are between strings
> - need large amounts of raw data

## Syntax-based approaches
Alternative: tree-to-tree alignment, generally based on constituency grammars. 

![](figures/parsetrees.png)

\pause

> - ~~"fixed" level of abstraction~~ work at all levels of abstraction 
> - correspondences are between ~~strings~~ grammatical objects
> - ~~need large amounts of raw data~~ work consistently well even on single _analyzed_ sentence pairs

## Syntax-based approaches: issues
> 1. grammars often defined independently, so not compatible each other
> 2. lack of robust parsers, while the quality of the analyses is crucial

## Grammatical Framework
- formalism/programming language to write __multilingual grammars__ $\to$ solves problem 1
  - one abstract syntax
  - multiple concrete syntaxes
  
\pause 

> - compilation-like approach to translation $\to$ good, grammaticality-preserving target language generation
  ![](figures/compiler.png)
> - but: problem 2 persist

## Universal Dependencies
- framework for cross-linguistically consistent grammatical annotation $\to$ same "multilingual" approach as GF \pause
- based on _dependency_, as opposed to constituency, relation
  - __dependency__: word-to-word correspondence
    - head
    - dependent in some relation with the head \pause
  ![](figures/boat_dep.png)
  
> - easier target for a parser (e.g. UDPipe) $\to$ solves problem 2
> - but: cannot be used for target language generation

## Solution: UD + GF
![](figures/proposed.png)

# Concept Extraction

## Representations of UD trees
![](figures/ud_formats.png)

- CoNNL-U is the standard format for UD trees \pause
- internally to the CA module, they are represented as rose trees \pause
  ```
  data RTree n = RTree n [RTree n]
  type UDTree = RTree UDWord
  type Alignment = (UDTree,UDTree)
  ```
    - `UDWord` represents a line of a CoNNL-u file 
  - `UDWord` represents a line of a CoNNL-u file 
    - `UDWord` represents a line of a CoNNL-u file 
  - `UDWord` represents a line of a CoNNL-u file 
    - `UDWord` represents a line of a CoNNL-u file 
    - alignments are pairs of ud trees

## Baseline
![](figures/rtrees.png) \pause

1. recursively sort trees based on the UD label of their root node (not needed in this case) \pause
2. pad the trees $\to$ perfectly aligned trees
   ![padding](figures/padded_rtrees.png) \pause
3. extract alignments: \pause
   - subtrees: $\langle$_she missed the boat, ha perso il treno_$\rangle$, $\langle$_the boat, il treno_$\rangle$, $\langle$_the,il_$\rangle$ \pause
   - heads:$\langle$_missed,perso_$\rangle$, $\langle$_boat,treno_$\rangle$

## Multiple criteria
- __label matching__ (original criterion): trees in matching context are aligned if they have the same UD label  \pause
- __POS-equivalence__: trees in matching context are aligned if they have the same multiset of POS tags of their _meaning-carrying_ words 
  - meaning-carrying words $\simeq$ content words \pause
- __known alignment__: trees in matching context are aligned if an equivalent alignment is already known
  - counting

## Divergences
__Divergence__: systematic cross-linguistic distinction. \pause

- categorial
  - _Gioara listens **distractedly**_ VS _Gioara lyssnar **distraherad**_
  - _Herbert completed his **doctoral** thesis_ VS _Herbert ha completato la sua tesi **di dottorato**_ \pause
- conflational
  - _Filippo is interested in **game development**_ VS _Filippo är intresserad av **spelutveckling**_ \pause
- structural
  - _I called **Francesco**_ VS _Ho telefonato **a Francesco**_ \pause
- head swapping
  - _Anna **usually** goes for walks_ VS _Anna **brukar** promenera_ \pause
- thematic
  - _**Yana** likes **books**_ VS _**A Yana** piacciono **i libri**_

## Head alignment
- extremely useful when alignment is perfect, like $\langle$_Claudio eats a banana,Claudio mangia una banana_$\rangle$
    - $\langle$_eats,mangia_$\rangle$
    - $\langle$_banana,banana_$\rangle$ \pause
- many problematic cases
  - some types of divergences $\to$ do not always align heads \pause
  - compounds & head verbs with auxiliaries $\to$ enhanced head alignment
    - $\langle$_many decisions were taken by Tommaso,många viktiga beslut togs av Tommaso$\rangle$ $\to$ $\langle$were taken,togs_$\rangle$
    - $\langle$_Giorgio took a course on machine learning techniques,Giorgio deltog i en kurs om maskininlärningstekniker_$\rangle$ $\to$ $\langle$_machine learning techniques,maskininlärningstekinker_$\rangle$

## Evaluation on PUD treebanks
__Against the baseline__
![](figures/e1.png)

\pause

__Against `fast_align`__
![](figures/e2.png)

## Evaluation on "raw" data
Data: sentence-aligned Computer Science course plans

- CSE (GU/Chalmers)
- DMI (UniPG)
  
![](figures/e3.png)

# Concept Propagation

## Two scenarios
![](figures/scenarios.png)

> 1. 3+ lingual parallel text
> 2. 2 bilingual parallel texts with one language in common

## General algorithm
For each $L_1$-$L_2$ alignment: \pause

> 1. look for its $L_2$ member among all subtrees of the $L_2$ version of the text where it is to be propagated
> 2. if it is present, align the sentence it belongs to with its _TL_ conterpart with the same procedure used for CE
> 3. if multiple candidate alignments are found, select the one with the closest depths

## Caveats
> - in step 1, irrelevant details of UD trees are to be ignored
  > - only consider word form, lemma, POS tag and dependency relation
> - head alignments require special treatment as they are not composed of subtrees

## Evaluation: scenario 1
![](figures/e4.png)

> - PUD treebanks
> - the vast majority of concepts is propagated



## Evaluation: scenario 2

__Texts in different domains__ (subsets of PUD treebanks)
![](figures/e5.png)

- mostly function words and very common function words 
  - $\langle$_always,alltid,sempre_$\rangle$

## Evaluation: scenario 2

__Texts in the same domain__ (course plans corpora)
![](figures/e6.png)

- domain-specific concepts \pause
  - $\langle$_skills, färdigheter, capacità_$\rangle$, $\langle$_exam, tentamen, prova_$\rangle$... \pause
  - $\langle$_the aim of the course, syftet med kursen, l'obiettivo del corso_$\rangle$ \pause
  - an interesting error: $\langle$_learning, inlärning, conoscere_$\rangle$

# MT experiments

## What's left 
![](figures/proposed_highlight.png)

## From UD to GF alignments
- UD alignment postprocessing:
  - normalization
  - selection based on size and usefulness \pause
- conversion of UD trees into GF ASTs via `gf-ud`
  - dependency configurations

## From alignments to a grammar

- aligned ASTs used to automatically generate a GF translation lexicon \pause
- again via one of `gf-ud`'s modules
  - requires: __extraction grammar__, __morphological dictionaries__ \pause
- grammar generating simple sentences, limited variation:
  - _the sentence is simple_
  - _a sentence is simple_
  - _sentences are simple_
  - _these sentences are simple_
  - _this sentence is an example_
  - _this short sentence is simple_
  - _this sentence of the text is simple_

## Extending the grammar
Easy to add RGL categories and functions to allow more variation:

> - _this sentence isn't simple_
> - _is this sentence simple?_
> - _this sentence was simple_
> - _this sentence will be simple_
> - _this sentence is simpler than that sentence_

\pause

Combining variations:

- _won't these short sentences be simpler than that long sentence?_

## Evaluation: strategy
- small course plans corpora $\to$ 2 bilingual lexica instead of a trilingual one \pause
- still small lexica + parsing issues $\to$ sentences to translate generated in the GF shell
  - partly arbitrary lexical and grammatical variations on a set of semantically plausible sentences \pause
- metric: BLEU scores \pause
- reference translations obtained by manual postprocessing of the automatic ones 
  - avoid low scores due to different but equally valid lexical choices

## Evaluation: results
![](figures/e7.png)

- max score:
  - $\langle$_the library provides useful textbooks, la biblioteca fornisce libri utili_$\rangle$
  - $\langle$_this lab is more difficult than the exam, den här laborationen är svårare än tentamen_$\rangle$ \pause
- min score:
  - $\langle$_the test is oral, la prova è dura_$\rangle$ \pause
- most errors are semantical, but 10% of the translation to Italian and 6% of those to Swedish only contain grammatical errors

# Conclusions and future work

## Conclusions
- developed a syntax-based CA module
  - Haskell library + easy to use and configure executables + evaluation and translation scripts \pause
- evaluation
    - against a baseline algorithm and a standard statistical tool
    - in a simple rule-based MT system 

## Future work
> - integration with statistical alignment techniques
> - verb phrases alignment
> - iterative CA
> - optimization of CP for multilingual corpora (scenario 1)
> - generalization of CE to $n$ languages
> - stricter and language pair-specific criteria
> - better alignment selection
