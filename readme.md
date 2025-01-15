# go-seqs

`go-seqs` is a library for working with sequence-like data in Go, taking advantage of generics introduced in Go 1.18.

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
Meanwhile, implementations of our [`seqs.Seq`](https://pkg.go.dev/github.com/siliconbrain/go-seqs/seqs#Seq) interface can also implement any other interface, e.g. optional length queries via [`seqs.Lener`](https://pkg.go.dev/github.com/siliconbrain/go-seqs/seqs#Lener)/[`seqs.FiniteSeq`](https://pkg.go.dev/github.com/siliconbrain/go-seqs/seqs#FiniteSeq).

Unfortunately, [`seqs.Seq`](https://pkg.go.dev/github.com/siliconbrain/go-seqs/seqs#Seq) is not directly compatible with [`iter.Seq`](https://pkg.go.dev/iter#Seq) since `yield`'s return value's meaning is inverted.