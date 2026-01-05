# go-seqs

`go-seqs` is a library for working with sequence-like data in Go, taking advantage of generics introduced in Go 1.18.

Starting with v1.0, `github.com/siliconbrain/go-seqs/iter` is a drop-in extension of `iter` with utilities for working with Go's standard sequences.

⚠️ NOTE: v1.0 breaks compatibility with previous versions by changing the definition of `seqs.Seq` to be more in line with how `iter` defines sequences.
`seqs.Seq` now requires `ForEachWhile(yield func(T) bool)` to be implemented, which is compatible with `iter.Seq`, instead of `ForEachUntil(yield func(T) bool)`, which defined the return value of `yield` inverted.
See the [migration guide](#migration-guide) for more information.

## Usage

Add the library as a dependency:
```sh
go get github.com/siliconbrain/go-seqs
```

Import it into your code:
```golang
import "github.com/siliconbrain/go-seqs/seqs"
```

Use to your heart's delight!

## Comparision with [`iter`](https://pkg.go.dev/iter)

The [`iter`](https://pkg.go.dev/iter) package defines [`Seq`](https://pkg.go.dev/iter#Seq) as
```golang
type Seq[V any] func(yield func(V) bool)
```
which unfortunately means no non-trivial methods can be defined on implementations.
Meanwhile, implementations of [`seqs.Seq`](https://pkg.go.dev/github.com/siliconbrain/go-seqs/seqs#Seq) interface can also implement any other interface.
The library exploits this to support
* querying the length of sequences without enumerating them (see [`seqs.FiniteSeq`](https://pkg.go.dev/github.com/siliconbrain/go-seqs/seqs#FiniteSeq)),
* quickly accessing specific items of directly indexable sequences (see [`seqs.Indexable`](https://pkg.go.dev/github.com/siliconbrain/go-seqs/seqs#Indexable)),
* marking and identifying sequences as infinite and/or possibly [divergent](https://en.wikipedia.org/wiki/Divergence_(computer_science)),
* and more.

## Migration guide

### v0.x → v1.x

To be compatible with `seqs.Seq`, types should implement `ForEachWhile` instead of `ForEachUntil` with the same signature but different semantics such that iteration should be aborted when `yield` returns `false`.

The following functions have been removed or renamed:
* `AppendTo` has been renamed to `AppendToSlice`.
* `ForEach`, `ForEachUntilWithIndex`, `ForEachWithIndex`, and `ForEachWhileWithIndex` have been removed in favor of using a `range-for` loop, with `iter.Enumerate(seqs.ToIter(seq))` if needed.
* `FromValue` has been renamed to `Singleton`.
* `GenerateWithIndex` has been removed, but can be easily replaced using `seqs.Map(seqs.Count(0, 1), gen)`.
* `PartialSums` has been renamed to `Sums`.
* `Reject` and `RejectWithIndex` have been removed to keep the number of functions under control; use `Filter` and `FilterWithIndex` instead respectively with an inverted predicate.
* The rarely used `RoundRobin` has been removed.
* `SeededReduce` and `SeededReductions` have been renamed to `Fold` and `Folds`, respectively.
* `SeqFunc` has been removed, use `FromIter` instead.
* `Skip` and `SkipWhile` have been renamed to `Drop` and `DropWhile`, respectively.
* The deprecated `SlidingWindow` has been removed.
* `ToIter2` has been removed, use `iter.UnpackMap(seqs.ToIter(seq), unpack)` instead.
* `ToSet` has been removed, use `maps.Collect(iter.Zip(seqs.ToIter(seq), iter.Repeat(true)))` instead.
