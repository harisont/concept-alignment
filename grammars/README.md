This folder contains all the "ingredients" for Italian and English grammar generation via gfud's [BuildGFGrammar](../../gfud/BuildGFGrammar.hs):

- the morphological dictionaries (copied from the GF RGL)
- the extraction grammars ([Extract.gf](Extract.gf) and its concrete syntaxes, copied from `gfud`)
- some test files with GF alignments like [this one](data/example-eng-ita-aligns.txt) which I assume can be obtained from `gfud -ud2gf ...` but I have no idea how