---
title: Grammar-Based Concept Alignment for Domain-Specific Machine Translation
subtitle: 08.09.2021
author: Arianna Masciolini and Aarne Ranta
theme: mhthm
---

## Context
- In __domain-specific MT__, precision is often more important than coverage \pause
- grammar-based pipelines (cf. GF) provide strong guarantees of __grammatical correctness__ \pause
- __lexical exactness__ is as important as grammaticality
  - need for high-quality __translation lexica__ preserving semantics _and_ morphological correctness

## Translation lexica
- Often built __manually__
  - __time__ consuming
  - significant __linguistic knowledge__ required \pause
- need for at least partial __automation__ 
  - __example parallel data__ required

## A parallel corpus
![](figures/alice_sentence.png)

From Lewis Carroll, _Alice's Adventures in Wonderland_. Parallel text at `paralleltext.io`

## Types of alignment
Word alignment:
![](figures/alice_word.png)


Phrase alignment: 
![](figures/alice_phrase.png)

## Approaches to automation

| __statistical__ (e.g. IBM models) | __syntax-based__ |
| --- | --- |
| require __large amounts of data__ | work consistently well even on __individual sentence pairs__
|||
| works with __raw__ data | requires the data to be __analyzed__ |
|||
| correspondences between __strings__ | correspondences between __grammatical objects__
|||
| "fixed" level of abstraction (__word__ or __phrase__) | all levels of abstraction $\to$ __concept__ alignment|

## Our approach
- Inconsistencies between different grammar formalisms $\to$ translation lexicon implemented in __GF__ \pause
- lack of robust constituency parsers while high-quality analysis is crucial $\to$ __UD__ parsing (UDPipe) \pause
- `gf-ud` for conversion

\pause

![](figures/high-level_overview.png)

## Universal Dependencies
![](figures/ud_formats.png)
Graphical, CoNNL-U and Rose Tree representation of the same UD tree.

- Framework for cross-linguistically consistent grammatical annotation \pause
- dependency-labelled links between words (head-dependent pairs) \pause
- cannot be used for target language generation

## Grammatical Framework
![](figures/parsetree.png)

- Constituency grammar formalism for __multilingual grammars__ (one abstract syntax + a concrete syntax per language)
- compilation-like translation (parsing + linearization)

# Concept Extraction

## Definitions

__*Concept*__: semantic unit of compositional translation expressed by a word or construction, conceived as a lemma equipped with morphological variations.

\pause

__*Alignment*__: tuple of equivalent concrete expressions in different languages; represents a concept.

## Extraction algorithm
![](figures/ce_alg.png)

## Matching UD labels
![](figures/boat_dep_labels.png)

- $\langle$_she missed the boat, ha perso il treno_$\rangle$ 
- $\langle$_missed the boat, perso il treno_$\rangle$ 
- *$\langle$_the boat, il treno_$\rangle$ 
- $\langle$_the, il_$\rangle$

\pause

Simple improvement: aligning heads of matching subtrees

- $\langle$_she missed the boat, ha perso il treno_$\rangle$, $\langle$_missed the boat, perso il treno_$\rangle$ $\to$ $\langle$_missed, ha perso_$\rangle$  (including the auxiliary)
- $\langle$_the boat, il treno_$\rangle$ 
$\to$ *$\langle$_boat, treno_$\rangle$ 


## POS equivalence
![](figures/boat_dep_pos.png) 

- more reliable __ignoring function words__ \pause
- in this case, basically same results as when matching labels \pause 
- can increase recall when labels do not coincide \pause
- can increase precision if used __in conjuncion with labels__

## Known translation divergence
__*Divergence*__: systematic cross-linguistic distinction. \pause

- categorial
  - $\langle$_Gioara listens **distractedly**_, _Gioara lyssnar **distraherad**_$\rangle$
  - $\langle$_Herbert completed his **doctoral** thesis_, _Herbert ha completato la sua tesi **di dottorato**_$\rangle$ 
- conflational
  - $\langle$_Filippo is interested in **game development**_, _Filippo är intresserad av **spelutveckling**_$\rangle$ 
- structural
  - $\langle$_I called **Francesco**_, _Ho telefonato **a Francesco**_$\rangle$ 
- head swapping
  - $\langle$_Anna **usually** goes for walks_, _Anna **brukar** promenera_$\rangle$  
- thematic
  - $\langle$_**Yana** likes **books**_, _**A Yana** piacciono **i libri**_$\rangle$

## Known alignment
- Allows using CA in conjunction with statistical tools \pause
- iterative application

## Searching for specific patterns
- `gf-ud` pattern matching to look for specific syntactic patterns \pause
- possible generalization via pattern replacement \pause

Example predication patterns:

- $\langle$_she missed the boat, ha perso il treno_$\rangle$ $\to$ $\langle$_[`subj`] missed [`obj`], ha perso [`obj`]_$\rangle$
- $\langle$_she told you that, hon berättade det för dig_$\rangle$ $\to$ $\langle$_[`subj`] told [`iobj`] [`obj`],[`subj`] berättade [`obj`] för [`obl`]_$\rangle$

# Grammar rules generation

## Requirements
- aligned UD trees \pause
- `gf-ud`  \pause
- __morphological dictionaries__ \pause
- __extraction grammar__

## Morphological dictionaries
Purely morphological unilingual dictionaries.

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

Example (prepositional NPs):

```
PrepNP : Prep -> NP -> PP # case head
```

## Lexical rules
Abstract:

```
fun in_the_field__inom_området_PP : PP ;
```

English concrete:

```
lin in_the_field__inom_område_PP = 
  PrepNP in_Prep (DetCN the_Det (UseN field_N))
```

# Evaluation

## Evaluating extraction
UD tree alignments are evaluated:

- independently from the quality of UD parsing (100-sentence subset of the manually annotated PUD corpus)
- on raw text (DMI and CSE course plans corpora)

\pause

Metrics: 

- % correct alignments
- % "useful" alignments

## Results on PUD corpus
\begin{table}[h]
  \centering
  \tiny
  \begin{tabular}{r|c|c|c|c|c|c}
  \multicolumn{1}{l|}{\textbf{}} & \multicolumn{2}{c|}{\textbf{CE}} & \multicolumn{2}{c|}{\textbf{\begin{tabular}[c]{@{}c@{}}\texttt{fast\_align} \\(100 sentences)\end{tabular}}} & \multicolumn{2}{c}{\textbf{\begin{tabular}[c]{@{}c@{}}\texttt{fast\_align} \\(full dataset)\end{tabular}}} \\ \hline
  \multicolumn{1}{l|}{}          & \textbf{en-it}  & \textbf{en-sv} & \textbf{en-it}                              & \textbf{en-sv}                              & \textbf{en-it}                                & \textbf{en-sv} \\ \hline  
  \textbf{distinct alignments}        & 536             & 638            & 1242                                        & 1044                                        & 1286                                          & 1065          \\ 
  \textbf{correct}                & 392 (73\%)      & 514 (80\%)     & 346 (28\%)                                  & 538 (52\%)                                  & 540 (42\%)                                    & 677 (64\%)    \\ 
  \textbf{usable in MT}                      & 363 (68\%)      & 503 (79\%)     & 316 (25\%)                                  & 525 (50\%)                                  & 510 (40\%)                                    & 666 (63\%)    \\ 
  \end{tabular}
  \end{table}

  \pause

  - CE module compared with `fast_align`, so extracting only one-to-many and many-to-one alignments \pause
  - CE has much higher precision, even when `fast_align` is trained on full 1000-sentence corpus

## Results on course plans corpora
\begin{table}[h]
  \centering
  \tiny
  \begin{tabular}{r|c|c|c|c}
  \multicolumn{1}{l|}{\textbf{}} & \multicolumn{2}{c|}{\textbf{PUD (100 sentences)}} & \multicolumn{2}{c}{\textbf{\begin{tabular}[c]{@{}c@{}}\textbf{course plans}\end{tabular}}} \\ \hline
  \multicolumn{1}{l|}{}          & \textbf{en-it}  & \textbf{en-sv} & \textbf{DMI (881 sentences)}                              & \textbf{CSE (539 sentences)} \\ \hline  
  \textbf{distinct alignments}        & 1197             & 1325            & 1823                                        & 1950                                        \\ 
  \textbf{correct}                & 916 (77\%)      & 1112 (85\%)     & 1205 (66\%)                                  & 1269 (66\%)                                     \\ 
  \textbf{usable in MT}                      & 880 (74\%)      & 1099 (84\%)     & 1157 (63\%)                                  & 1248 (64\%)                                  \\ 
  \end{tabular}
  \end{table}

  \pause

  - Comparison between experiments on manually annotated treebanks and raw text \pause
  - precision decreases, but is still higher than `fast_align`'s \pause
  - recall much lower

## MT experiments
- No need for an _ad hoc_ grammar: extend extraction grammar with existing RGL functions \pause
- 2 bilingual lexica from course plans corpora \pause
- corpus of sentences to translate generated in the GF shell
  - semi-random lexical and grammatical variations on a set of semantically plausible sentences \pause
- metric: BLEU scores \pause
- reference translations obtained by manual postprocessing of the automatic ones \pause
  - avoid low scores due to different but equally valid lexical and grammatical choices

## Results
\begin{table}[h]
  \centering
  \scriptsize
  \begin{tabular}{r|cc}
  \textbf{}            & \textbf{DMI (en-it)} & \textbf{CSE (en-sv)} \\ \hline
  \textbf{BLEU-1 to 4} & 55         & 61         \\ 
  \textbf{BLEU-1 to 3} & 63         & 68         \\ 
  \textbf{BLEU-1 to 2} & 70         & 74          \\
  \textbf{BLEU-1}      & 79         & 81         \\ 
  \end{tabular}
  \label{tableu}
\end{table}

\pause

- Better results for English-Swedish (due to systematic errors in Italian) \pause
- sentence-level scores range from 0 (sometimes due to a single semantic error) to 100

## Conclusions
- Extraction technique performing consistently well even on small datasets \pause
- simultaneous extraction of word, phrase, ... alignments, incl. discontinuous expressions \pause
- possibility to search for specific types of correspondences, e.g. predication patterns \pause
- customizable divergence patterns \pause
- output: compilable, morphology-aware GF translation lexica \pause
- require manual corrections and completions, but can significantly reduce lexicon bootstrapping time \pause
- available as Haskell library + executables

## Current and future work

- Concept Propagation \pause
  - same text in new language (equivalent to multilingual CE) \pause
  - new text in new language (within same domain) \pause
- integration with statistical tools \pause
- postprocessing tools