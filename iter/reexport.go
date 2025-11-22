package iter

import "iter"

type Seq[V any] = iter.Seq[V]
type Seq2[K, V any] = iter.Seq2[K, V]

func Pull[V any](seq Seq[V]) (next func() (V, bool), stop func()) {
	return iter.Pull(seq)
}

func Pull2[K, V any](seq Seq2[K, V]) (next func() (K, V, bool), stop func()) {
	return iter.Pull2(seq)
}
