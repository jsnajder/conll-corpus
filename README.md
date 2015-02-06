# conll-corpus

This module implements the reader for [CoNLL](http://ilk.uvt.nl/conll/) corpus
format and a utility for counting wordforms and lemmas from corpus in CoNLL
format. The utility supports Map/Reduce for [Hadoop
streaming](http://hadoop.apache.org/docs/r1.2.1/streaming.html#Hadoop+Streaming).

## Installation

Install the [Haskell platform](https://www.haskell.org/platform/) and upgrade
[Cabal](https://www.haskell.org/cabal/download.html) to the newest stable
version.

Get the source:

```bash
$ git clone https://github.com/jsnajder/sdewac-prepro sdewac-prepro
```

Get the following dependencies (not yet on Hackage):

```bash
$ git clone http://github.com/jsnajder/counts counts
$ git clone http://github.com/jsnajder/counts conll-corpus
```

Then build from source:

```bash
$ cd sdewac-prepro
$ cabal sandbox init
$ cabal sanbox add-source ../counts
$ cabal sanbox add-source ../conll-corpus
$ cabal install --only-dependencies
$ cabal configure
$ cabal build --builddir=bin
```
## Usage example

First fix MATE-parsed CoNLL output:

```bash
$ cd sdewac-prepro/data
$ ../bin/fix-mate-conll.sh sdewac-mate.sample.conll-bogus > sdewac-mate.sample.conll
```

Counting lemmas in MST-parsed and MATE-parsed corpus, appending CPOSTAG to lemmas:

```bash
$ ../bin/conll2counts -p sdewac-mst.sample.conll > sdewac-mst.sample.lemmas
$ ../bin/conll2counts -p sdewac-mate.sample.conll > sdewac-mate.sample.lemmas
```

Testing the map-reduce version:

```bash
$ ../bin/conll2counts -m -p < sdewac-mst.sample.conll | sort | ../bin/conll2counts -r
```

