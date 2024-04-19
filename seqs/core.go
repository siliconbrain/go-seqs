package seqs

import (
	"slices"
)

// Seq defines a minimal interface for a sequence of values
type Seq[E any] interface {
	// ForEachUntil calles the specified function for each sequence element until the function returns `true`
	ForEachUntil(yield func(E) bool)
}

// Lener is an interface optionally implemented by sequences which have a finite length
type Lener interface {
	// Len returns the length of a collection
	//
	// This method returns an `int` to be consistent with the `len()` built-in function.
	Len() int
}

// FiniteSeq is a Seq that has a finite length
type FiniteSeq[E any] interface {
	Seq[E]
	Lener
}

// All returns true if the specified predicate returns true for all elements of the specified sequence
//
// Application to an infinite sequence will block indefinitely.
func All[S Seq[E], E any](seq S, pred func(E) bool) (res bool) {
	return And(Map(seq, pred))
}

// And returns true if all elements of the specified sequence are true, which includes the empty sequence
//
// Application to an infinite sequence will block indefinitely.
func And[S Seq[bool]](seq S) bool {
	res := true
	ForEachWhile(seq, func(b bool) bool {
		res = res && bool(b)
		return res
	})
	return res
}

// Any returns true if the specified predicate returns true for any element of the specified sequence
//
// Application to an infinite sequence will block indefinitely.
func Any[S Seq[E], E any](seq S, pred func(E) bool) (res bool) {
	return Or(Map(seq, pred))
}

// AppendTo appends elements from the specified sequence to the slice(ish)
func AppendTo[S Seq[E], E any, Es ~[]E](seq S, slice Es) Es {
	if lener, ok := asLener(seq); ok {
		slices.Grow(slice, lener.Len())
	}
	ForEach(seq, func(e E) {
		slice = append(slice, e)
	})
	return slice
}

// Concat returns a sequence that is the concatenation of the specified sequences
func Concat[E any](seqs ...Seq[E]) Seq[E] {
	switch len(seqs) {
	case 0:
		return Empty[E]()
	case 1:
		return seqs[0]
	default:
		forEachUntil := func(yield func(E) bool) {
			brk := false
			for _, seq := range seqs {
				seq.ForEachUntil(func(e E) bool {
					brk = yield(e)
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
		return SeqFunc(forEachUntil)
	}
}

// Count returns the number of elements in the specified sequence.
//
// Application to an infinite sequence will block indefinitely.
func Count[S Seq[E], E any](seq S) int {
	return Sum(Map(seq, func(E) int { return 1 }))
}

// Cycle returns an (almost always) infinite sequence that cyclically repeats the elements of the specified sequence
//
// If the specified sequence is empty (or becomes empty at any point) the returned sequence becomes empty to avoid an unbreakable infinite loop.
func Cycle[S Seq[E], E any](seq S) Seq[E] {
	return SeqFunc(func(yield func(E) bool) {
		brk := false
		for {
			empty := true
			seq.ForEachUntil(func(e E) bool {
				empty = false
				brk = yield(e)
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
func Filter[S Seq[E], E any](seq S, pred func(E) bool) Seq[E] {
	return FilterWithIndex(seq, func(_ int, e E) bool {
		return pred(e)
	})
}

// FilterWithIndex returns a sequence that only contains elements of the specified sequence for which the specified predicate returns `true`
func FilterWithIndex[S Seq[E], E any](seq S, pred func(int, E) bool) Seq[E] {
	return SeqFunc(func(yield func(E) bool) {
		idx := 0
		seq.ForEachUntil(func(e E) bool {
			cond := pred(idx, e)
			idx++
			if cond {
				return yield(e)
			}
			return false
		})
	})
}

// First returns the first element of the specified sequence and true, or the zero value of E and false if the sequence is empty.
func First[S Seq[E], E any](seq S) (first E, hasFirst bool) {
	seq.ForEachUntil(func(e E) bool {
		first, hasFirst = e, true
		return true
	})
	return
}

// Flatten returns a sequence that is the concatenation of sequences contained by the specified sequence
//
// The returned sequence never implements the Lener interface. Use Concat with ToSlice if you want the resulting sequence to implement Lener when possible.
func Flatten[SS Seq[S], S Seq[E], E any](seq SS) Seq[E] {
	// NOTE: We could support Lener by checking if all subsequences implement Lener (like with Concat),
	//       but looping through the super sequence might incur a nontrivial performance hit.
	return SeqFunc(func(yield func(E) bool) {
		brk := false
		seq.ForEachUntil(func(s S) bool {
			s.ForEachUntil(func(e E) bool {
				brk = yield(e)
				return brk
			})
			return brk
		})
	})
}

// ForEach calls the specified function for each element of the specified sequence
//
// Application to an infinite sequence will block indefinitely.
func ForEach[S Seq[E], E any](seq S, yield func(E)) {
	seq.ForEachUntil(func(e E) bool {
		yield(e)
		return false
	})
}

// ForEachUntilWithIndex calls the specified function for each element of the specified sequence along with its index until the function returns `true`
func ForEachUntilWithIndex[S Seq[E], E any](seq S, yield func(int, E) bool) {
	idx := 0
	seq.ForEachUntil(func(e E) bool {
		res := yield(idx, e)
		idx++
		return res
	})
}

// ForEachWithIndex calls the specified function for each element of the specified sequence along with its index
func ForEachWithIndex[S Seq[E], E any](seq S, yield func(int, E)) {
	ForEachUntilWithIndex(seq, func(i int, e E) bool {
		yield(i, e)
		return false
	})
}

// ForEachWhile calls the specified function for each element of the specified sequence while the function returns `true`
func ForEachWhile[S Seq[E], E any](seq S, yield func(E) bool) {
	seq.ForEachUntil(func(e E) bool {
		return !yield(e)
	})
}

// ForEachWhileWithIndex calls the specified function for each element of the specified sequence along with its index while the function returns `true`
func ForEachWhileWithIndex[S Seq[E], E any](seq S, yield func(int, E) bool) {
	idx := 0
	ForEachWhile(seq, func(e E) bool {
		res := yield(idx, e)
		idx++
		return res
	})
}

// FromValues returns a sequence made up of the specified values
func FromValues[E any](values ...E) Seq[E] {
	return FromSlice(values)
}

// FromSlice returns a sequence whose elemets are (copies of) the specified slice's elements
func FromSlice[Es ~[]E, E any](s Es) Seq[E] {
	return sliceSeq[E](s)
}

// FromSlicePtrs returns a sequence whose elements are pointers to the specified slice's elements
func FromSlicePtrs[Es ~[]E, E any](s Es) Seq[*E] {
	return slicePtrsSeq[E](s)
}

// Generate returns a sequence whose elements are generated by calling the provided function
func Generate[E any](gen func() E) Seq[E] {
	return SeqFunc(func(yield func(E) bool) {
		for !yield(gen()) {
		}
	})
}

// GenerateWithIndex returns a sequence whose elements are generated by calling the provided function with the current index
func GenerateWithIndex[E any](gen func(idx int) E) Seq[E] {
	return SeqFunc(func(yield func(E) bool) {
		for i := 0; !yield(gen(i)); i++ {
		}
	})
}

// Intersperse returns a sequence whose elements are the same as the specified sequence's but interspersed with the specified value
func Intersperse[S Seq[E], E any](seq S, val E) Seq[E] {
	forEachUntil := func(yield func(E) bool) {
		first := true
		seq.ForEachUntil(func(e E) bool {
			if !first {
				if yield(val) {
					return true
				}
			} else {
				first = false
			}
			return yield(e)
		})
	}
	if lener, ok := asLener(seq); ok {
		return seqFuncWithLen[E]{
			forEachUntil: forEachUntil,
			len:          func() int { return max(lener.Len()*2-1, 0) },
		}
	}
	return SeqFunc(forEachUntil)
}

// IsEmpty returns true if the specified sequence has no elements; otherwise, it returns false.
func IsEmpty[S Seq[E], E any](seq S) bool {
	if lener, ok := asLener(seq); ok {
		return lener.Len() == 0
	}
	_, hasFirst := First(seq)
	return !hasFirst
}

// Last returns the last element of the specified sequence and true, or the zero value of E and false if the sequence is empty.
//
// Application to an infinite sequence will block indefinitely.
func Last[S Seq[E], E any](seq S) (last E, hasLast bool) {
	ForEach(seq, func(e E) {
		last, hasLast = e, true
	})
	return
}

// Map returns a sequence whose elements are obtained by applying the specified mapping function to elements of the specified sequence
func Map[S Seq[Src], Src any, Dst any](seq S, mapfn func(Src) Dst) Seq[Dst] {
	return MapWithIndex(seq, func(_ int, e Src) Dst {
		return mapfn(e)
	})
}

// MapWithIndex returns a sequence whose elements are obtained by applying the specified mapping function to elements of the specified sequence along with their indices
func MapWithIndex[S Seq[Src], Src any, Dst any](seq S, mapfn func(int, Src) Dst) Seq[Dst] {
	forEachUntil := func(yield func(Dst) bool) {
		idx := 0
		seq.ForEachUntil(func(src Src) bool {
			res := yield(mapfn(idx, src))
			idx++
			return res
		})
	}

	if lener, ok := asLener(seq); ok {
		return seqFuncWithLen[Dst]{
			forEachUntil: forEachUntil,
			len:          lener.Len, // same length as seq
		}
	}
	return SeqFunc(forEachUntil)
}

// Or returns true if any element of the specified sequence is true, which does not include the empty sequence
//
// Application to an infinite sequence will block indefinitely.
func Or[S Seq[bool]](seq S) bool {
	res := false
	seq.ForEachUntil(func(b bool) bool {
		res = res || b
		return res
	})
	return res
}

// PartialSums returns a sequence of the partial sums of the specified sequence.
func PartialSums[S Seq[E], E Summable](seq S) Seq[E] {
	return Reductions(seq, add)
}

// Reduce returns a value obtained by aggregating elements of the specified sequence using the specified operation.
//
// If the specified sequence is empty, the zero value of E will be returned.
// If the specified sequence has a single element only, that element will be returned.
//
// Application to an infinite sequence will block indefinitely.
func Reduce[S Seq[E], E any](seq S, op func(E, E) E) (res E) {
	first := true
	ForEach(seq, func(e E) {
		if first {
			first = false
			res = e
		} else {
			res = op(res, e)
		}
	})
	return
}

// Reductions returns the sequence of intermediate values of the reduction of the specified sequence with the specified operation.
//
// If the specified sequence is empty, the returned sequence will be empty.
// If the specified sequence has a single element only, a sequence containing only that element will be returned.
func Reductions[S Seq[E], E any](seq S, op func(E, E) E) Seq[E] {
	forEachUntil := func(yield func(E) bool) {
		var acc E
		first := true
		seq.ForEachUntil(func(e E) bool {
			if first {
				first = false
				acc = e
			} else {
				acc = op(acc, e)
			}
			return yield(acc)
		})
	}

	if lener, ok := asLener(seq); ok {
		return seqFuncWithLen[E]{
			forEachUntil: forEachUntil,
			len:          lener.Len, // same length as seq
		}
	}
	return SeqFunc(forEachUntil)
}

// Reject returns a sequence that only contains elements of the specified sequence for which the specified predicate returns `false`
func Reject[S Seq[E], E any](seq S, pred func(E) bool) Seq[E] {
	return RejectWithIndex(seq, func(_ int, e E) bool {
		return pred(e)
	})
}

// RejectWithIndex returns a sequence that only contains elements of the specified sequence for which the specified predicate returns `false`
func RejectWithIndex[S Seq[E], E any](seq S, pred func(int, E) bool) Seq[E] {
	return FilterWithIndex(seq, func(i int, e E) bool {
		return !pred(i, e)
	})
}

// Repeat returns an infinite sequence that repeats the specified value
func Repeat[E any](e E) Seq[E] {
	return SeqFunc(func(yield func(E) bool) {
		for {
			if yield(e) {
				return
			}
		}
	})
}

// Repeat returns a finite sequence that repeats the specified value `n` times
func RepeatN[E any](e E, n int) Seq[E] {
	// NOTE: cannot implement as Take(Repeat(e), n) because Take is (currently) unable to detect that its sequence is infinite an thus provide a correct length
	return seqFuncWithLen[E]{
		forEachUntil: func(yield func(E) bool) {
			for i := 0; i < n; i++ {
				if yield(e) {
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
	}

	forEachUntil := func(yield func(E) bool) {
		nextChs := make([]chan E, len(seqs))
		moveNextChs := make([]chan struct{}, len(seqs))

		for i := range seqs {
			nextChs[i] = make(chan E)
			moveNextChs[i] = make(chan struct{})

			defer close(moveNextChs[i])

			go iterate(seqs[i], nextChs[i], moveNextChs[i])
		}

		live := len(seqs)
		for idx := 0; live > 0; idx = (idx + 1) % len(seqs) {
			nextCh := nextChs[idx]
			if nextCh == nil {
				continue
			}
			moveNextChs[idx] <- struct{}{}
			next, hasNext := <-nextCh
			if !hasNext {
				nextChs[idx] = nil
				live--
				continue
			}
			if yield(next) {
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
	return SeqFunc(forEachUntil)
}

// SeededReduce returns a value obtained by applying the specified function to an accumlator value (initialized to the specified seed value) and successive elements of the sequence
//
// Application to an infinite sequence will block indefinitely.
func SeededReduce[S Seq[E], E any, A any](seq S, seed A, op func(A, E) A) (res A) {
	res = seed
	ForEach(seq, func(e E) {
		res = op(res, e)
	})
	return
}

// SeededReductions returns the sequence of intermediate values of the reduction of the specified sequence by the specified operation starting with the specified seed value.
//
// The returned sequence always has the seed value as its first element.
func SeededReductions[S Seq[E], E any, A any](seq S, seed A, op func(A, E) A) Seq[A] {
	forEachUntil := func(yield func(A) bool) {
		acc := seed
		if yield(acc) {
			return
		}
		seq.ForEachUntil(func(e E) bool {
			acc = op(acc, e)
			return yield(acc)
		})
	}

	if lener, ok := asLener(seq); ok {
		return seqFuncWithLen[A]{
			forEachUntil: forEachUntil,
			len:          func() int { return 1 + lener.Len() },
		}
	}
	return SeqFunc(forEachUntil)
}

// SeqFunc returns a sequence that has its ForEachUntil method implemented by the specified function
func SeqFunc[E any](fn func(yield func(E) bool)) Seq[E] {
	return seqFunc[E](fn)
}

// Skip returns a sequence that omits the first `n` number of elements of the specified sequence
//
// If the source sequence has less than `n` elements the returned sequence will be empty
func Skip[S Seq[E], E any](s S, n int) Seq[E] {
	if n == 0 {
		return s
	}
	forEachUntil := func(yield func(E) bool) {
		i := 0
		s.ForEachUntil(func(e E) bool {
			if i < n {
				i++
				return false
			}
			return yield(e)
		})
	}
	if lener, ok := asLener(s); ok {
		return seqFuncWithLen[E]{
			forEachUntil: forEachUntil,
			len:          func() int { return max(lener.Len()-n, 0) },
		}
	}
	return SeqFunc(forEachUntil)
}

// SkipWhile returns a sequence that omits elements of the specified sequence while the specified predicate returns `true`
func SkipWhile[S Seq[E], E any](s S, pred func(E) bool) Seq[E] {
	return SeqFunc(func(yield func(E) bool) {
		skipping := true
		s.ForEachUntil(func(e E) bool {
			if !skipping {
				return yield(e)
			}
			skipping = pred(e)
			if !skipping {
				return yield(e)
			}
			return false
		})
	})
}

// SlidingWindow returns a sequence of windows (slices of count length) containing the elements of the specified sequence.
// Windows start after each other with a distance of skip.
func SlidingWindow[S Seq[E], E any](seq S, count int, skip int) (res Seq[[]E]) {
	if count < 1 {
		panic("count must be positive")
	}
	if skip < 1 {
		panic("skip must be positive")
	}
	forEachUntil := func(yield func([]E) bool) {
		shift := min(skip, count)
		ignore := skip - shift

		buf := make([]E, 0, count)
		ignoreCnt := 0
		seq.ForEachUntil(func(e E) bool {
			if ignoreCnt > 0 {
				ignoreCnt--
				return false
			}
			buf = append(buf, e)
			if len(buf) == count {
				if yield(slices.Clone(buf)) {
					return true
				}
				buf = append(buf[0:0], buf[shift:]...)
				ignoreCnt = ignore // reset ignore counter
			}
			return false
		})
	}
	if lener, ok := asLener(seq); ok {
		return seqFuncWithLen[[]E]{
			forEachUntil: forEachUntil,
			len: func() int {
				return (lener.Len() - count + 1) / skip
			},
		}
	}
	return SeqFunc(forEachUntil)
}

// Sum returns the sum of the specified sequence's elements.
//
// Application to an infinite sequence will block indefinitely.
func Sum[S Seq[E], E Summable](seq S) E {
	return Reduce(seq, add)
}

// Summable lists types that support addition using the + operator
type Summable interface {
	~float32 | ~float64 | ~int | ~int8 | ~int16 | ~int32 | ~int64 | ~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | string
}

// Take returns a sequence of the first `n` number of elements of the specified sequence
//
// If the source sequence has less then `n` elements the returned sequence will also have only that many elements
func Take[S Seq[E], E any](s S, n int) Seq[E] {
	if n == 0 {
		return Empty[E]()
	}

	forEachUntil := func(yield func(E) bool) {
		cnt := 0
		s.ForEachUntil(func(e E) bool {
			cnt++
			return yield(e) || cnt == n
		})
	}
	if lener, ok := asLener(s); ok {
		return seqFuncWithLen[E]{
			forEachUntil: forEachUntil,
			len:          func() int { return min(lener.Len(), n) },
		}
	}
	return SeqFunc(forEachUntil)
}

// TakeWhile returns a sequence of the first elements of the specified sequence while the specified predicate returns `true`
//
// TakeWhile(FromValues())
func TakeWhile[S Seq[E], E any](s S, pred func(E) bool) Seq[E] {
	return SeqFunc(func(yield func(E) bool) {
		s.ForEachUntil(func(e E) bool {
			if pred(e) {
				return yield(e)
			}
			return true
		})
	})
}

// ToSet returns a set (boolean valued map) created from the elements of the specified sequence
//
// Application to an infinite sequence will block indefinitely.
func ToSet[S Seq[E], E comparable](seq S) (res map[E]bool) {
	if lener, ok := asLener(seq); ok {
		res = make(map[E]bool, lener.Len())
	} else {
		res = make(map[E]bool)
	}
	seq.ForEachUntil(func(e E) bool {
		res[e] = true
		return false
	})
	return
}

// ToSlice returns a slice of the specified sequence's elements
func ToSlice[S Seq[E], E any](seq S) (res []E) {
	if lener, ok := asLener(seq); ok {
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

// ZipMany returns a sequence of slices containing elements of the same index from the specified sequences
func ZipMany[E any](seqs ...Seq[E]) Seq[[]E] {
	switch len(seqs) {
	case 0:
		return Empty[[]E]()
	case 1:
		return Map(seqs[0], func(e E) []E { return []E{e} })
	}

	forEachUntil := func(yield func([]E) bool) {
		nextChs := make([]chan E, len(seqs))
		moveNextChs := make([]chan struct{}, len(seqs))

		for i := range seqs {
			nextChs[i] = make(chan E)
			moveNextChs[i] = make(chan struct{})

			defer close(moveNextChs[i])

			go iterate(seqs[i], nextChs[i], moveNextChs[i])
		}

		for {
			for _, moveNextCh := range moveNextChs {
				moveNextCh <- struct{}{}
			}

			nexts := make([]E, len(seqs))
			allHaveNext := true
			for i, nextCh := range nextChs {
				next, hasNext := <-nextCh
				nexts[i] = next
				allHaveNext = allHaveNext && hasNext
			}

			if !allHaveNext || yield(nexts) {
				return
			}
		}
	}
	if leners(seqs...) {
		return seqFuncWithLen[[]E]{
			forEachUntil: forEachUntil,
			len: func() (length int) {
				for _, seq := range seqs {
					length = min(length, seq.(Lener).Len())
				}
				return
			},
		}
	}
	return SeqFunc(forEachUntil)
}

// ZipWith returns a sequence containing results of the specified merge function applied to elements of the same index from both specified sequences
func ZipWith[S1 Seq[E1], S2 Seq[E2], E1 any, E2 any, T any](seq1 S1, seq2 S2, merge func(E1, E2) T) Seq[T] {
	forEachUntil := func(yield func(T) bool) {
		nextCh1, nextCh2 := make(chan E1), make(chan E2)
		moveNextCh1, moveNextCh2 := make(chan struct{}), make(chan struct{})

		// closing moveNextCh* channels releases the below goroutines
		defer close(moveNextCh1)
		defer close(moveNextCh2)

		go iterate(seq1, nextCh1, moveNextCh1)
		go iterate(seq2, nextCh2, moveNextCh2)

		for {
			moveNextCh1 <- struct{}{}
			moveNextCh2 <- struct{}{}

			next1, hasNext1 := <-nextCh1
			next2, hasNext2 := <-nextCh2

			if !hasNext1 || !hasNext2 || yield(merge(next1, next2)) {
				return
			}
		}
	}
	if lener1, ok := asLener(seq1); ok {
		if lener2, ok := asLener(seq2); ok {
			return seqFuncWithLen[T]{
				forEachUntil: forEachUntil,
				len:          func() int { return min(lener1.Len(), lener2.Len()) },
			}
		}
	}
	return SeqFunc(forEachUntil)
}

func iterate[E any](seq Seq[E], nextCh chan<- E, moveNextCh <-chan struct{}) {
	defer close(nextCh)

	_, moveNext := <-moveNextCh
	if !moveNext {
		return
	}

	ForEachWhile(seq, func(e E) bool {
		nextCh <- e
		_, moveNext := <-moveNextCh
		return moveNext
	})
}

type emptySeq[E any] struct{}

func (emptySeq[E]) ForEachUntil(func(E) bool) {}

func (emptySeq[E]) Len() int {
	return 0
}

type seqFunc[E any] func(yield func(E) bool)

func (s seqFunc[E]) ForEachUntil(yield func(E) bool) {
	s(yield)
}

type seqFuncWithLen[E any] struct {
	forEachUntil seqFunc[E]
	len          func() int
}

func (s seqFuncWithLen[E]) ForEachUntil(yield func(E) bool) {
	s.forEachUntil(yield)
}

func (s seqFuncWithLen[E]) Len() int {
	return s.len()
}

type sliceSeq[E any] []E

func (s sliceSeq[E]) ForEachUntil(yield func(E) bool) {
	for _, e := range s {
		if yield(e) {
			return
		}
	}
}

func (s sliceSeq[E]) Len() int {
	return len(s)
}

type slicePtrsSeq[E any] []E

func (s slicePtrsSeq[E]) ForEachUntil(yield func(*E) bool) {
	for i := range s {
		if yield(&s[i]) {
			return
		}
	}
}

func (s slicePtrsSeq[E]) Len() int {
	return len(s)
}

func add[V Summable](a, b V) V {
	return a + b
}

func asLener(v any) (Lener, bool) {
	lener, ok := v.(Lener)
	return lener, ok
}

func leners[E any](seqs ...Seq[E]) bool {
	for _, seq := range seqs {
		if _, ok := seq.(Lener); !ok {
			return false
		}
	}
	return true
}
