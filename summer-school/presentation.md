---
title: Concept Alignment for Multilingual Machine Translation
subtitle: 04.07.2021
author: Arianna Masciolini
theme: mhthm
---

## Context
- GF is well suited for __domain-specific MT__ systems, where precision is more 
  important than coverage, as it provides strong guarantees of grammatical 
  correctness \pause
- in such systems, __lexical exactness__ is as important as grammaticality \pause
  - need for high-quality __translation lexica__ preserving semantics _and_
    morphological correctness

## The problem
- manually building a translation lexicon
  - is time consuming
  - requires significant linguistic knowledge
- desire to __automate__ this process at least in part \pause
  - possible when __example parallel data__ are available

## A parallel corpus
![](figures/alice_sentence.png)

From Lewis Carroll, _Alice's adventures in Wonderland_. Parallel text at `paralleltext.io`

## Alignment
Word alignment:
![](figures/alice_word.png)

\pause

Phrase alignment: 
![](figures/alice_phrase.png)

## Statistical approaches
Standard approaches are statistical (IBM models).

\pause

- __pros__: \pause
  - easy to use \pause
  - can handle noisy data \pause
  - fast on large corpora \pause
- __cons__: \pause
  - _require_ large amounts of raw data \pause
  - correspondences between strings \pause $\to$ no morphological info \pause
  - "fixed" level of abstraction (word or phrase)

## Syntax-based approaches I
Alternative: tree-to-tree alignment.

![](figures/parsetrees1.png)

## Syntax-based approaches II
Word alignment

![](figures/parsetrees2.png)

## Syntax-based approaches III
Phrase alignment

![](figures/parsetrees3.png)

## Comparison

| __statistical__ | __syntax-based__ |
| --- | --- |
| require __large amounts of data__ | work consistently well even on __individual sentence pairs__
|||
| works with __raw__ data | requires the data to be __analyzed__ |
|||
| correspondences between __strings__ | correspondences between __grammatical objects__
|||
| "fixed" level of abstraction (__word__ or __phrase__) | all levels of abstraction $\to$ __concept__ alignment|


## Why not just use GF?
- quality of the analysis is crucial
  - lack of robust GF parsers \pause
- dependency trees are an easier target for a parser
  - robust parsers such as UDPipe

## Overview
![](figures/high-level_overview.png)

\pause

1. parse parallel data to UD trees \pause
2. search for aligned UD subtrees \pause
3. convert them to GF trees and then grammar rules

## UD trees
![](figures/ud_formats.png)
Graphical, CoNNL-U and Rose Tree representation of the same UD tree.

- dependency-labelled links between words (head-dependent pairs) \pause
- POS tags \pause
- ...

# Extracting concepts

## Matching dependency labels
![](figures/boat_dep_labels.png)

\pause

- $\langle$_she missed the boat, ha perso il treno_$\rangle$ \pause
- $\langle$_missed the boat, perso il treno_$\rangle$ \pause
- *$\langle$_the boat, il treno_$\rangle$ \pause
- $\langle$_the, il_$\rangle$

## Aligning heads of maching trees

- *$\langle$_the boat, il treno_$\rangle$ \pause
$\to$ *$\langle$_boat, treno_$\rangle$ \pause
- $\langle$_missed the boat, perso il treno_$\rangle$ \pause $\to$ $\langle$_missed, ha perso_$\rangle$ \pause (including the auxiliary)

## Using POS tags
![](figures/boat_dep_pos.png) \pause

- more reliable __ignoring function words__ \pause
- in this case, basically same results as when matching labels \pause
- can increase recall when labels do not coincide \pause
- can increase precision if used __in conjuncion with labels__

## Translation divergences
__Divergence__: systematic cross-linguistic distinction. \pause

- categorial
  - $\langle$_Gioara listens **distractedly**_, _Gioara lyssnar **distraherad**_$\rangle$
  - $\langle$_Herbert completed his **doctoral** thesis_, _Herbert ha completato la sua tesi **di dottorato**_$\rangle$ \pause
- conflational
  - $\langle$_Filippo is interested in **game development**_, _Filippo är intresserad av **spelutveckling**_$\rangle$ \pause
- structural
  - $\langle$_I called **Francesco**_, _Ho telefonato **a Francesco**_$\rangle$ \pause
- head swapping
  - $\langle$_Anna **usually** goes for walks_, _Anna **brukar** promenera_$\rangle$ \pause 
- thematic
  - $\langle$_**Yana** likes **books**_, _**A Yana** piacciono **i libri**_$\rangle$

## Reusing known alignments
- allows using CA in conjunction with statistical tools \pause
- iterative application

## Searching for specific patterns
- `gf-ud` pattern matching allows looking for specific syntactic patterns \pause
- possible generalization via pattern replacement \pause

Example predication patterns:

- $\langle$_[`subj`] missed [`obj`],[`subj`] ha perso [`obj`]_$\rangle$
- $\langle$_[`subj`] told [`iobj`] [`obj`],[`subj`] berättade [`obj`] för [`obl`]_$\rangle$

# Propagating concepts to a new language

## Concept Propagation
- So far, we focused on how to identify correspondences in bilingual parallel texts (**_Concept Extraction_**) \pause
- what happens when we need to handle a third language?
  - **_Concept Propagation_**: finding expression corresponding to a known concept in a new language

## Scenario 1
![](figures/s1.png)

## Scenario 2
![](figures/s2.png)

# Generating grammar rules

## Requirements
- aligned UD trees \pause
- morphological dictionaries \pause
- extraction grammar 

## Morphological dictionaries
Purely morphological unilingual dictionaries.

\pause

Example:

```
...
lin morphologic_A = 
  mkAMost "morphologic" "morphologicly" ;
lin morphological_A = 
  mkAMost "morphological" "morphologically" ;
lin morphology_N = 
  mkN "morphology" "morphologies" ;
...
```

## Extraction grammar
Defines the syntactic categories and functions to build lexical entries. 

\pause

Example (prepositional NPs):

```
PrepNP : Prep -> NP -> PP # case head
```

## Lexical rules
Abstract:

```
fun in_the_field__inom_området_PP : PP ;
```

\pause

English concrete:

```
lin in_the_field__inom_område_PP = 
  PrepNP in_Prep (DetCN the_Det (UseN field_N))
```

## Refining the generated lexicon
- interactive selection
- CoNNL-U synoptic viewer

## Detailed view
![](figures/full_overview.png)

## Summary
- concept extraction (UD) \pause
- concept propagation (UD) \pause
- GF lexicon generation \pause
- postprocessing tools

# Questions?