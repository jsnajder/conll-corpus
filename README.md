# conll-corpus

## Installation

Install the [Haskell platform](https://www.haskell.org/platform/) and upgrade
[Cabal](https://www.haskell.org/cabal/download.html) to the newest stable
version.

Get the source:

> mkdir <root>/sdewac-prepro
> git clone https://github.com/jsnajder/sdewac-prepro <root>/sdewac-prepro

Get the following dependencies (not yet on Hackage):

> mkdir <root>/counts
> git clone http://github.com/jsnajder/counts <root>/counts

> mkdir <root>/conll-corpus
> git clone http://github.com/jsnajder/counts <root>/conll-corpus

Build from source:

> cd <root>/sdewac-prepro
> cabal sandbox init
> cabal sanbox add-source <root>/counts
> cabal sanbox add-source <root>/conll-corpus
> cabal install --only-dependencies
> cabal configure
> cabal build --builddir=bin

## Usage example

Fix MATE-parsed CoNLL output:

> cd <root>/data
> ../bin/fix-mate-conll.sh sdewac-mate.sample.conll-bogus > sdewac-mate.sample.conll

Count lemmas in MST-parsed and MATE-parsed corpus, appending CPOSTAG to lemmas:

> ../bin/conll2counts -p sdewac-mst.sample.conll > sdewac-mst.sample.lemmas
> ../bin/conll2counts -p sdewac-mate.sample.conll > sdewac-mate.sample.lemmas

Testing the map-reduce version:

> ../bin/conll2counts -m -p < sdewac-mst.sample.conll | sort | ../bin/conll2counts -r

