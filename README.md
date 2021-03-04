# Syntax-based Concept Alignment for Machine Translation

This repository contains the code, [report](final_report/Main.pdf) and presentation [slides](presentation/presentation.pdf) of my Master's thesis project (Göteborgs Univeristet, A.Y. 2020-21).

---
## Installation

To compile the CA module, the Haskell Stack is recommended. To build
the project, clone this repository, move into the
corresponding directory and run `stack build`. This will generate a
Haskell library, named `concept-alignment`, and the following five
executables:

-   `extract-concepts` (for CE)
-   `propagate-concepts` (for CP)
-   `evalign`, a script for evaluating CP and CE
-   `generate-grammar`, to automatically generate a GF grammar from the alignments extraced via CE and/or CP
-   `translate`, to perform simple MT experiments

## Usage

### `extract-concepts`

Using the CE module with the default parameters is straightforward:

```
stack exec -- extract-concepts SL.conllu TL.conllu
```

The program, however, supports a number of command-line options. Most
importantly:

-   `–file=FILE` can be used to specify where to write the resulting
    alignments. Unless otherwise specified (see below), the output
    consists in two new aligned `.conllu` files, stored at the chosen
    location, whose names are prefixed with `SL` and `TL` respectively
-   `–linearize` specifies that concepts should be linearized
-   `–maxsize=INT` sets the maximum size of the extracted alignments
-   `-pattern=FILE` specifies the path to a `.hst` defining a specific
    pattern the extracted alignments have to match. The syntax of `.hst` files is described [here](https://github.com/GrammaticalFramework/gf-ud/tree/fa1fe1977e80d435c7eaad8e230489a0306b3e4c#some-examples-of-use) (`pattern-match`)
-   `–all` specifies that the selection step should be skipped
-   `–clauses` enables clause segmentation
-   `–rest` enables a second pass of alignment "by exclusion"
-   `–pharaoh=FILE` is used to specify a file of alignments in pharaoh format to use as backup.

### `propagate-concepts`

CP requires two, instead of three, `.conllu` files, one with the
concepts to propagate and the following two containing the corpus of
annotated sentences where they should be looked for. As a consequence,
running it with the default parameters is as follows:

```
stack exec -- propagate-concepts SL_concepts.conllu SL.conllu TL.conllu
```

All `extract-concepts` options that are also relevant for CP (i.e. all
of them excepts `–maxsize` and `pharaoh`) are also valid for this second
executable.

### `evalign`

The evaluation script `evalign` can be run in three different "modes":

1.  single-file mode (`stack exec – evalign linearized.txt`):
    given a pre-annotated linearized file, it prints out basic statistics about
    precision, recall and amount of reusable alignments.
2.  extraction mode
    (`stack exec – evalign extraction old.txt new.txt`):
    given an annotated and a new, possibly yet-to-annotate file
    containing linearized alignments, it allows interactive minimal
    annotation of the latter (if needed) and, on top of printing out the
    basic statistics, it compares the new alignments to the old ones,
    telling how many correct and incorrect alignments were lost and/or
    found
3.  propagation mode
    (`stack exec – evalign propagation new.txt new.txt`): similar to
    extraction interactive mode, excepts that the statistics are
    CP-specific (percentage of successfully propagated alignments,
    number of errors introduced by CP etc.).

The command line option `–reasons` can be added when criterion-wise
statistics are needed.

### `generate-grammar`

The grammar generation module can be run as follows:

```
stack exec -- generate-grammar path_to_extract_grammar paths_to_morphodicts paths_to_aligned_conllu_files
```

Here, `path_to_extract_grammar` should point to the extraction grammar
in `.pgf` format. `paths_to_morphodicts` stands for the path of the
morphological dictionaries of the languages involved, also in `.pgf`
format.

Similarly, `paths_to_aligned_conllu_files` is to be replaced with the
files containing the concepts in CoNNL-u format. These files should be,
in terms of the languages they are written in, in the same order as the
morphological dictionaries.

### `translate`

The simplicity of the translation module makes it also extremely easy to
use. The string or list of newline-separate strings to translate comes
from the standard input and the only argument is the path to the
automatically generated GF grammar to be used, again in `.pgf` format.
Here is an example of using the program to translate a single sentence:

```
echo "this sentence will be translated" | stack exec -- translate
Extracted.pgf             
```
## Configuration: modifying the alignment criteria

Modifying the criteria CA makes use of requires
little effort. All criteria are in fact defined in a separate Haskell
module `Criteria` that only exports the list of criteria to be used.
In the current implementation, it looks like this:

```haskell
criteria :: [Criterion]
criteria = [udpos, ud, divs, pass, pos]
```

Removing and/or changing the priority of the criteria is then just a
matter of altering such list.

Adding new criteria is simple too, but it also requires an understanding
of the data type `Criterion`, defined in module `ConceptAlignment`:

```haskell
    data Criterion = C {
        func :: UDTree -> UDTree -> Bool, 
        reas :: S.Set Reason,
        headAlign :: Bool,
        strict :: Bool
}
```

As the above code block shows, `Criterion` is a record type whose fields
are:

-   a boolean function `func` specifying a rule to decide whether two
    trees should be aligned. For instance, UD label matching is implemented as

```haskell
    udMatch :: UDTree -> UDTree -> Bool
    (RTree n ts) `udMatch` (RTree m us) = 
        udDEPREL n == udDEPREL m
```

-   a set of "reasons" `reas` to be used for ranking the alignments,
    shown in the linearized files and used to compute criterion-specific
    statistics. `Reason`s, and not alignment rules (`func`s), are what
    mirrors alignment criteria
    exactly. A `func` can define in fact a more
    specific alignment rule, such as "type of categorial divergence in
    which an adjective is replaced by adverb", while a `Reason` is in
    practice a more coarse-grained label associated to potentially many
    rules

-   two boolean flags, `headAlign` and `strict`, the former specifying
    whether head alignment should
    be performed for UD tree pairs matching that criterion, the latter
    marking the criterion as either strict (i.e. to be also used for
    "alignment by exclusion" or not.
