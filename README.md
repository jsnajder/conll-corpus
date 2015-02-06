# conll-corpus

*2015-02-06*

This package implements a reader for [CoNLL](http://ilk.uvt.nl/conll/) corpus
format and a tool for counting wordforms and lemmas from corpus in CoNLL
format. 

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

## `conll2counts` tool

This tool counts the wordforms or the lemmas from a corpus in the ConLL format.

```
conll2counts: usage: conll2counts [options] [<filename>]
  [-m,--map]             run as hadoop mapper (reads from stdin)
  [-r,--reduce]          run as hadoop reducer (reads from stdin)
  [-w,--wordforms]       count wordforms
  [-l,--lemmas]          count lemmas (default)
  [-L,--lemmas-backoff]  count lemmas with backoff to wordforms for <unknown> lemmas
  [-p,--pos]             append coarse-grained part-of-speech tag (CPOSTAG) to wordform/lemma
  [<filename>]           corpus in CoNLL format
```

### Usage example

Counting lemmas in MST-parsed sDeWaC corpus, with appending of CPOSTAG to
lemmas:

```
$ cd data
$ ../bin/conll2counts -p sdewac-mst.sample.conll > sdewac-mst.sample.lemmas
$ ../bin/conll2counts -p sdewac-mate.sample.conll > sdewac-mate.sample.lemmas
```

The tool can be run via [Hadoop Map/Reduce
streaming](http://hadoop.apache.org/docs/r1.2.1/streaming.html#Hadoop+Streaming),
by using `-m` and `-r` flags for the mapper and reducer, respectively. The
following simulates this:

```
$ ../bin/conll2counts -m -p < sdewac-mst.sample.conll | sort | ../bin/conll2counts -r
```

