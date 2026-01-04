package iter

import (
	"iter"

	"github.com/siliconbrain/go-seqs/internal"
)

type Pred[Value any] = internal.Pred[Value]
type Pred2[Value1, Value2 any] = internal.Pred2[Value1, Value2]
type Seq[V any] = iter.Seq[V]
type Seq2[K, V any] = iter.Seq2[K, V]
type Summable = internal.Summable

func Pull[V any](seq Seq[V]) (next func() (V, bool), stop func()) {
	return iter.Pull(seq)
}

func Pull2[K, V any](seq Seq2[K, V]) (next func() (K, V, bool), stop func()) {
	return iter.Pull2(seq)
}
