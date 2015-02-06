# conll-corpus

* Jan 6, 2015 *

This module implements the reader for [CoNLL](http://ilk.uvt.nl/conll/) corpus
format and a utility for counting wordforms and lemmas from corpus in CoNLL
format. The utility supports Map/Reduce for [Hadoop
streaming](http://hadoop.apache.org/docs/r1.2.1/streaming.html#Hadoop+Streaming).

## Installation

Install the [Haskell platform](https://www.haskell.org/platform/) and upgrade
[Cabal](https://www.haskell.org/cabal/download.html) to the newest stable
version.

Get the source:

```
$ git clone http://github.com/jsnajder/conll-corpus conll-corpus
```

Get the following dependency (which is not on Hackage):

```
$ git clone http://github.com/jsnajder/counts counts
```

Then build from the source:

```
$ cd conll-corpus
$ cabal sandbox init
$ cabal sandbox add-source ../counts
$ cabal install --only-dependencies
$ cabal configure
$ cabal install --bindir=bin
```

## Usage example

Counting lemmas in MST-parsed and MATE-parsed sDeWaC corpus, thereby appending
CPOSTAG to lemmas:

```
$ cd data
$ ../bin/conll2counts -p sdewac-mst.sample.conll > sdewac-mst.sample.lemmas
$ ../bin/conll2counts -p sdewac-mate.sample.conll > sdewac-mate.sample.lemmas
```

Testing the map-reduce version:

```
$ ../bin/conll2counts -m -p < sdewac-mst.sample.conll | sort | ../bin/conll2counts -r
```

