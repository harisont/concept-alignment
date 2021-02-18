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
- __Concept Extraction__: identifying new concepts via linguistic comparison
- __Concept Propagation__: finding expressions corresponding to known concepts in a particular language

## CA in translation
A human translator

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

Issues:

- "fixed" level of abstraction (generally either word or phrase alignment)
- correspondences are between strings
- need large amounts of raw data

## Syntax-based approaches
Alternative: tree-to-tree alignment, generally based on constituency grammars. 

![MISSING FIGURE (parse trees + micorgrammar)](parse_trees.png)

- ~~"fixed" level of abstraction~~ work at all levels of abstraction simultaneously 
- correspondences are between ~~strings~~ grammatical objects
- ~~need large amounts of raw data~~ work consistently well even on single _analyzed_ sentence pairs

## Syntax-based approaches: issues
1. grammars often defined independently, so not compatible each other
2. lack of robust parsers, while the quality of the analyses is crucial

## Grammatical Framework
- formalism/programming language to write __multilingual grammars__ $\to$ solves problem 1
  - one abstract syntax
  - multiple concrete syntaxes
- compilation-like approach to translation $\to$ good, grammaticality-preserving target language generation
  ![](figures/compiler.png)
- but: problem 2 persist

## Universal Dependencies
- framework for cross-linguistically consistent grammatical annotation $\to$ same "multilingual" approach as GF
- based on _dependency_, as opposed to constituency, relation
  - __dependency__: word-to-word correspondence
    - head
    - dependent in some relation with the head
  ![MISSING FIGURE same parse tree as before (EN) + corresponding UD](figures/GFvUD.png)
- easier target for a parser (e.g. UDPipe) $\to$ solves problem 2
- but: cannot be used for target language generation

## Solution: UD + GF
![](figures/proposed.png)