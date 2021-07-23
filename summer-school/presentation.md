---
title: Concept Alignment for Multilingual Machine Translation
subtitle: 04.07.2021
author: Arianna Masciolini
theme: mhthm
---

## Context
- GF is well suited for domain-specific MT systems where precision is more 
  important than coverage, as it provides strong guarantees of grammatical 
  correctness
- in such systems, __lexical exactness__ is as important as grammaticality 
  - need for high-quality __translation lexica__ preserving semantics _and_
    morphological correctness

## The problem
- manually building a translation lexicon
  - is time consuming
  - requires significant linguistic knowledge
- desire to __automate__ this process at least in part
  - possible when __example parallel data__ are available

## A parallel corpus
![](figures/alice_sentence.png)

From Lewis Carroll, _Alice's adventures in Wonderland_. Parallel text at `paralleltext.io`

## Alignment
Word alignment:
![](figures/alice_word.png)

Phrase alignment: 
![](figures/alice_phrase.png)

## Statistical approaches
Standard approaches are statistical (IBM models).

- __Pros__:
  - easy to use
  - can handle noisy data
  - fast on large corpora
- __Cons__:
  - _require_ large amounts of raw data
  - correspondences between strings $\to$ no morphological info
  - "fixed" level of abstraction (word or phrase)

## Syntax-based approaches I
Alternative: tree-to-tree alignment.

![](figures/parsetrees1.png)

## Syntax-based approaches II
Alternative: tree-to-tree alignment.

![](figures/parsetrees2.png)

## Syntax-based approaches III
Alternative: tree-to-tree alignment.

![](figures/parsetrees3.png)

## Comparison

| __statistical__ | __syntax-based__ |
| --- | --- |
| require large amounts of raw data | work even on single _analyzed_ sentence pairs
| correspondences between strings | correspondences between grammatical objects
| "fixed" level of abstraction | all levels of abstraction $\to$ __concept__ alignment|


## Why not just use GF?
- quality of the analysis is crucial
  - lack of robust GF parsers
- dependency trees are an easier target for a parser
  - robust parsers such as UDPipe

## Overview
![](figures/high-level_overview.png)

1. parse parallel data to UD trees
2. search for aligned UD subtrees
3. convert them to GF trees and then grammar rules

## UD trees
![](figures/ud_formats.png)
Graphical, CoNNL-U and Rose Tree representation of the same UD tree.

- dependency-labelled links between words (head-dependent pairs)
- POS tags
- ...

# Extracting concepts

## Matching dependency labels
![](figures/boat_dep_labels.png)

## Aligning heads of maching trees

## Using POS tags
![](figures/boat_dep_pos.png)

## Reusing known alignments

## Translation divergences

## Searching for specific patterns

# Propagating concepts to a new language

## Scenario 1

## Scenario 2

## Detailed overview

# Generating grammar rules

## Requirements

## Morphological dictionaries

## Extraction grammar

## Lexical rules

# Refining the generated lexicon

## Interactive selection

## Postprocessing

# Conclusions

## Summary

# Questions?