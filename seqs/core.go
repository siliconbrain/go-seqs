package seqs

import (
	"sync/atomic"

	"golang.org/x/exp/constraints"
)

// Seq defines a minimal interface for a sequence of values
type Seq[E any] interface {
	// ForEachUntil calles the specified function for each sequence element until the function returns `true`
	ForEachUntil(fn func(E) bool)
}

// Lener is an interface optionally implemented by sequences which have a finite length
type Lener interface {
	// Len returns the length of a collection
	//
	// This method returns an `int` to be consistent with the `len()` built-in function.
	Len() int
}

// LenerSeq is a combination of the Seq and Lener interfaces
type LenerSeq[E any] interface {
	Seq[E]
	Lener
}

// Concat returns a sequence that is the concatenation of the specified sequences
func Concat[E any](seqs ...Seq[E]) Seq[E] {
	switch len(seqs) {
	case 0:
		return Empty[E]()
	case 1:
		return seqs[0]
	default:
		forEachUntil := func(fn func(E) bool) {
			brk := false
			for _, seq := range seqs {
				seq.ForEachUntil(func(e E) bool {
					brk = fn(e)
					return brk
				})
				if brk {
					return
				}
			}
		}
		if leners(seqs...) {
			return seqFuncWithLen[E]{
				forEachUntil: forEachUntil,
				len: func() (l int) {
					for _, seq := range seqs {
						l += seq.(Lener).Len()
					}
					return
				},
			}
		}
		return seqFunc[E](forEachUntil)
	}
}

// Cycle returns an (almost always) infinite sequence that cyclically repeats the elements of the specified sequence
//
// If the specified sequence is empty (or becomes empty at any point) the returned sequence becomes empty to avoid an unbreakable infinite loop.
func Cycle[E any](seq Seq[E]) Seq[E] {
	return seqFunc[E](func(fn func(E) bool) {
		brk := false
		for {
			empty := true
			seq.ForEachUntil(func(e E) bool {
				empty = false
				brk = fn(e)
				return brk
			})
			if brk || empty {
				return
			}
		}
	})
}

// Empty returns an empty sequence
func Empty[E any]() Seq[E] {
	return emptySeq[E]{}
}

// Filter returns a sequence that only contains elements of the specified sequence for which the specified predicate returns `true`
func Filter[E any](seq Seq[E], pred func(E) bool) Seq[E] {
	return seqFunc[E](func(fn func(E) bool) {
		seq.ForEachUntil(func(e E) bool {
			if pred(e) {
				return fn(e)
			}
			return false
		})
	})
}

// ForEach calls the specified function for each element of the specified sequence
func ForEach[E any](seq Seq[E], fn func(E)) {
	seq.ForEachUntil(func(e E) bool {
		fn(e)
		return false
	})
}

// ForEachWhile calls the specified function for each element of the specified sequence while the function returns `true`
func ForEachWhile[E any](seq Seq[E], fn func(E) bool) {
	seq.ForEachUntil(func(e E) bool {
		return !fn(e)
	})
}

// FromValues returns a sequence made up of the specified values
func FromValues[E any](values ...E) Seq[E] {
	return FromSlice(values)
}

// FromSlice adapts a slice to implement the Seq interface
func FromSlice[S ~[]E, E any](s S) Seq[E] {
	return sliceSeq[E](s)
}

// Intersperse returns a sequence whose elements are the same as the specified sequence's but interspersed with the specified value
func Intersperse[E any](seq Seq[E], val E) Seq[E] {
	forEachUntil := func(fn func(E) bool) {
		first := true
		seq.ForEachUntil(func(e E) bool {
			if !first {
				if fn(val) {
					return true
				}
			} else {
				first = false
			}
			return fn(e)
		})
	}
	if lener, ok := seq.(Lener); ok {
		return seqFuncWithLen[E]{
			forEachUntil: forEachUntil,
			len:          func() int { return max(lener.Len()*2-1, 0) },
		}
	}
	return seqFunc[E](forEachUntil)
}

// Map returns a sequence whose elements are obtained by applying the specified mapping function to elements of the specified sequence
func Map[Src any, Dst any](seq Seq[Src], mapfn func(Src) Dst) Seq[Dst] {
	forEachUntil := func(fn func(Dst) bool) {
		seq.ForEachUntil(func(src Src) bool {
			return fn(mapfn(src))
		})
	}

	if lener, ok := seq.(Lener); ok {
		return seqFuncWithLen[Dst]{
			forEachUntil: forEachUntil,
			len:          lener.Len, // same length as seq
		}
	}
	return seqFunc[Dst](forEachUntil)
}

// Reduce returns a value obtained by applying the specified function to an accumlator value (initialized with the specified seed value) and successive elements of the sequence
func Reduce[E any, A any](seq Seq[E], seed A, fn func(A, E) A) (res A) {
	res = seed
	ForEach(seq, func(e E) {
		res = fn(res, e)
	})
	return
}

// Repeat returns an infinite sequence that repeats the specified value
func Repeat[E any](e E) Seq[E] {
	return seqFunc[E](func(fn func(E) bool) {
		for {
			if fn(e) {
				return
			}
		}
	})
}

// Repeat returns a finite sequence that repeats the specified value `n` times
func RepeatN[E any](e E, n int) Seq[E] {
	return seqFuncWithLen[E]{
		forEachUntil: func(fn func(E) bool) {
			for i := 0; i < n; i++ {
				if fn(e) {
					return
				}
			}
		},
		len: func() int { return n },
	}
}

// RoundRobin returns a sequence whose elements are obtained by alternately taking elements from the specified sequences in a round-robin fashion
func RoundRobin[E any](seqs ...Seq[E]) Seq[E] {
	switch len(seqs) {
	case 0:
		return Empty[E]()
	case 1:
		return seqs[0]
	default:
		forEachUntil := func(fn func(E) bool) {
			recChs := make([]chan E, len(seqs))
			stopCh := make(chan struct{})
			live := int32(len(recChs))
			for i := range seqs {
				seq := seqs[i]
				recCh := make(chan E)
				go func() {
					seq.ForEachUntil(func(e E) bool {
						select {
						case <-stopCh:
							return true
						case recCh <- e:
							return false
						}
					})
					close(recCh)
					atomic.AddInt32(&live, -1)
				}()
				recChs[i] = recCh
			}
			i := 0
			for {
				if atomic.LoadInt32(&live) == 0 {
					return
				}
				if e, ok := <-recChs[i]; ok {
					if fn(e) {
						close(stopCh)
						return
					}
				}
				i = (i + 1) % len(recChs)
			}
		}
		if leners(seqs...) {
			return seqFuncWithLen[E]{
				forEachUntil: forEachUntil,
				len: func() (l int) {
					for _, seq := range seqs {
						l += seq.(Lener).Len()
					}
					return
				},
			}
		}
		return seqFunc[E](forEachUntil)
	}
}

// Skip returns a sequence that omits the first `n` number of elements of the specified sequence
//
// If the source sequence has less than `n` elements the returned sequence will be empty
func Skip[E any](s Seq[E], n int) Seq[E] {
	if n == 0 {
		return s
	}
	forEachUntil := func(fn func(E) bool) {
		i := 0
		s.ForEachUntil(func(e E) bool {
			if i < n {
				i++
				return false
			}
			return fn(e)
		})
	}
	if lener, ok := s.(Lener); ok {
		return seqFuncWithLen[E]{
			forEachUntil: forEachUntil,
			len:          func() int { return max(lener.Len()-n, 0) },
		}
	}
	return seqFunc[E](forEachUntil)
}

// SkipWhile returns a sequence that omits elements of the specified sequence while the specified predicate returns `true`
func SkipWhile[E any](s Seq[E], pred func(E) bool) Seq[E] {
	return seqFunc[E](func(fn func(E) bool) {
		skipping := true
		s.ForEachUntil(func(e E) bool {
			if !skipping {
				return fn(e)
			}
			skipping = pred(e)
			if !skipping {
				return fn(e)
			}
			return false
		})
	})
}

// Take returns a sequence of the first `n` number of elements of the specified sequence
//
// If the source sequence has less then `n` elements the returned sequence will also have only that many elements
func Take[E any](s Seq[E], n int) Seq[E] {
	forEachUntil := func(fn func(E) bool) {
		i := 0
		s.ForEachUntil(func(e E) bool {
			if i < n {
				i++
				return fn(e)
			}
			return true
		})
	}
	if lener, ok := s.(Lener); ok {
		return seqFuncWithLen[E]{
			forEachUntil: forEachUntil,
			len:          func() int { return min(lener.Len(), n) },
		}
	}
	return seqFunc[E](forEachUntil)
}

// TakeWhile returns a sequence of the first elements of the specified sequence while the specified predicate returns `true`
//
// TakeWhile(FromValues())
func TakeWhile[E any](s Seq[E], pred func(E) bool) Seq[E] {
	return seqFunc[E](func(fn func(E) bool) {
		s.ForEachUntil(func(e E) bool {
			if pred(e) {
				return fn(e)
			}
			return true
		})
	})
}

// ToSet returns a set (boolean valued map) created from the elements of the specified sequence
func ToSet[E comparable](seq Seq[E]) (res map[E]bool) {
	if lener, ok := seq.(Lener); ok {
		res = make(map[E]bool, lener.Len())
	}
	seq.ForEachUntil(func(e E) bool {
		res[e] = true
		return false
	})
	return
}

// ToSlice returns a slice of the specified sequence's elements
func ToSlice[E any](seq Seq[E]) (res []E) {
	if lener, ok := seq.(Lener); ok {
		if l := lener.Len(); l > 0 {
			res = make([]E, 0, lener.Len())
		}
	}
	seq.ForEachUntil(func(e E) bool {
		res = append(res, e)
		return false
	})
	return
}

type emptySeq[E any] struct{}

func (emptySeq[E]) ForEachUntil(func(E) bool) {}

func (emptySeq[E]) Len() int {
	return 0
}

type seqFunc[E any] func(fn func(E) bool)

func (s seqFunc[E]) ForEachUntil(fn func(E) bool) {
	s(fn)
}

type seqFuncWithLen[E any] struct {
	forEachUntil seqFunc[E]
	len          func() int
}

func (s seqFuncWithLen[E]) ForEachUntil(fn func(E) bool) {
	s.forEachUntil(fn)
}

func (s seqFuncWithLen[E]) Len() int {
	return s.len()
}

type sliceSeq[E any] []E

func (s sliceSeq[E]) ForEachUntil(fn func(E) bool) {
	for _, e := range s {
		if fn(e) {
			return
		}
	}
}

func (s sliceSeq[E]) Len() int {
	return len(s)
}

func max[V constraints.Ordered](a, b V) V {
	if a < b {
		return b
	}
	return a
}

func min[V constraints.Ordered](a, b V) V {
	if a < b {
		return a
	}
	return b
}

func leners[E any](seqs ...Seq[E]) bool {
	for _, seq := range seqs {
		if _, ok := seq.(Lener); !ok {
			return false
		}
	}
	return true
}
