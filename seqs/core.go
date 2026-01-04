package seqs

import (
	"cmp"
	"errors"
	"slices"

	"github.com/siliconbrain/go-seqs/internal"
	int_iter "github.com/siliconbrain/go-seqs/internal/iter"
	"github.com/siliconbrain/go-seqs/iter"
)

// All returns whether the specified predicate matches all items in the specified sequence.
//
// Application to an infinite sequence might block indefinitely.
func All[Item any](seq Seq[Item], pred Pred[Item]) bool {
	return iter.All(ToIter(seq), pred)
}

// And returns the logical AND of the boolean values in the specified sequence.
// The evaluation is short-circuiting.
//
// Application to an infinite sequence might block indefinitely.
func And[SeqOfBool Seq[bool]](seq SeqOfBool) bool {
	return iter.And(ToIter(seq))
}

// Any returns whether the specified predicate matches any items in the specified sequence.
//
// Application to an infinite sequence might block indefinitely.
func Any[Item any](seq Seq[Item], pred Pred[Item]) bool {
	return iter.Any(ToIter(seq), pred)
}

// AppendToSlice appends items from the specified sequence to the specified slice(ish).
//
// If the sequence is known to be infinite, the function panics.
// If the sequence has a known length, the slice is grown to have enough space before appending.
func AppendToSlice[SeqOfItem Seq[Item], Slice ~[]Item, Item any](seq SeqOfItem, slice Slice) Slice {
	if IsInfinite(seq) {
		panic(errors.New("infinitely many items cannot be appended to slice"))
	}
	if len, hasLen := getLength(seq); hasLen {
		slice = slices.Grow(slice, len)
	}
	return slices.AppendSeq(slice, ToIter(seq))
}

// AsFiniteSeq tries to type cast the specified sequence as a finite sequence.
func AsFiniteSeq[SeqOfItem Seq[Item], Item any](seq SeqOfItem) (FiniteSeq[Item], bool) {
	finite, ok := any(seq).(FiniteSeq[Item])
	return finite, ok
}

// AsSeq type casts a concrete sequence as a generic sequence
func AsSeq[SeqOfItem Seq[Item], Item any](seq SeqOfItem) Seq[Item] {
	return seq
}

// CanDiverge returns true when the specified sequence is marked as possibly divergent, i.e. it might be impossible to terminate its enumeration.
//
// For example, calling [Filter] on a (known-to-be) infinite sequence always returns a possibly divergent sequence
// since [Filter] might need to look at an infinite number of items before it can yield one that matches its predicate.
func CanDiverge[SeqOfItem Seq[Item], Item any](seq SeqOfItem) bool {
	_, canDiverge := any(seq).(interface{ CanDiverge() })
	return canDiverge
}

// Cartesian returns a sequence of pairs where each pair is a member of the cartesian product of
// (i.e. all combinations of items from) the two specified sequences.
func Cartesian[ItemIn1, ItemIn2, ItemOut any](seq1 Seq[ItemIn1], seq2 Seq[ItemIn2], combine func(ItemIn1, ItemIn2) ItemOut) Seq[ItemOut] {
	res := FromIter(iter.PackMap(iter.Cartesian(ToIter(seq1), ToIter(seq2)), combine))

	finiteSeq1, isFiniteSeq1 := AsFiniteSeq(seq1)
	finiteSeq2, isFiniteSeq2 := AsFiniteSeq(seq2)
	if isFiniteSeq1 && isFiniteSeq2 {
		res = makeFinite(res, func() int {
			return finiteSeq1.Len() * finiteSeq2.Len()
		})
	} else if IsInfinite(seq1) || IsInfinite(seq2) {
		res = markInfinite(res)
	}
	return res
}

// Concat returns a sequence of items that is the concatenation of items from the specified sequences.
func Concat[Item any](seqs ...Seq[Item]) Seq[Item] {
	return concat(wrapSeqSlice(seqs))
}

// Count returns a sequence of repeatedly adding step to the previous value, starting with from.
func Count[Item Summable](from Item, step Item) Seq[Item] {
	// NOTE: Even though none of the summable types are infinite, the sequence will be. Overflows or OOM don't change that.
	return markInfinite(FromIter(iter.Count(from, step)))
}

// Cycle returns a sequence of items that infinitely (1) repeates the specified sequence.
//
// (1): If the specified sequence is empty (or becomes empty at any point) the returned sequence becomes empty to avoid an unbreakable infinite loop.
func Cycle[SeqOfItem Seq[Item], Item any](seq SeqOfItem) Seq[Item] {
	if IsInfinite(seq) {
		// an infinite sequence cannot be repeated
		return seq
	}
	if isEmpty, weKnow := looksEmpty(seq); weKnow && isEmpty {
		return Empty[Item]()
	}
	return FromIter(iter.Cycle(ToIter(seq)))
}

// Divvy returns a sequence of slices with at most size length containing a continuous range of items from the specified sequence.
// The start of each slice will be offset by skip number of items from the previous one.
// Slices will overlap when size > skip, and some items will be dropped when size < skip.
func Divvy[Item any](seq Seq[Item], size int, skip int) Seq[[]Item] {
	res := FromIter(iter.Divvy(ToIter(seq), size, skip))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, func() int {
			l := finiteSeq.Len()
			return roundUpDiv(min(l, size), size) + roundUpDiv(max(l-size, 0), skip)
		})
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// DivvyExact is like [Divvy] but all yielded slices are exactly size length.
// Any trailing items are dropped.
func DivvyExact[Item any](seq Seq[Item], size int, skip int) Seq[[]Item] {
	res := FromIter(iter.DivvyExact(ToIter(seq), size, skip))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, func() int {
			l := finiteSeq.Len()
			return roundDownDiv(min(l, size), size) + roundDownDiv(max(l-size, 0), skip)
		})
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// Drop returns a sequence with at most n items dropped from the start of the specified sequence.
func Drop[Item any](seq Seq[Item], n int) Seq[Item] {
	if n == 0 {
		return seq
	}

	res := FromIter(iter.Drop(ToIter(seq), n))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, func() int { return max(finiteSeq.Len()-n, 0) })
	} else if IsInfinite(seq) {
		res = markInfinite(seq)
	}
	return res
}

// DropLast returns the specified sequence of items without its last n items.
// When the specified sequence has less than n items, the result will be empty.
//
// Uses O(n) space.
func DropLast[Item any](seq Seq[Item], n int) Seq[Item] {
	if n < 1 {
		return seq
	}
	if IsInfinite(seq) {
		// cannot drop the last n items of an infinite sequence
		return seq
	}
	if isEmpty, weKnow := looksEmpty(seq); weKnow && isEmpty {
		return Empty[Item]()
	}
	res := FromIter(iter.DropLast(ToIter(seq), n))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, func() int { return max(finiteSeq.Len()-n, 0) })
	}
	return res
}

// DropWhile returns the rest of the specified sequence after the prefix of items matching the specified predicate.
func DropWhile[Item any](seq Seq[Item], pred Pred[Item]) Seq[Item] {
	return FromIter(iter.DropWhile(ToIter(seq), pred))
}

// Empty returns an empty sequence.
func Empty[Item any]() Seq[Item] {
	return emptySeq[Item]{}
}

// Enumerate returns a sequence of pairs where each pair consists of the ordinal of an item from the specified sequence and the item itself.
func Enumerate[SeqOfItem Seq[Item], Item any](seq SeqOfItem) Seq[Pair[int, Item]] {
	res := FromIter2(iter.Enumerate(ToIter(seq)), pairFrom)
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, finiteSeq.Len)
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// Filter returns a sequence of items that only contains items of the specified sequence that match the specified predicate.
func Filter[Item any](seq Seq[Item], pred Pred[Item]) Seq[Item] {
	res := FromIter(iter.Filter(ToIter(seq), pred))
	if IsInfinite(seq) {
		res = struct {
			Seq[Item]
			canDivergeMark
			infiniteMark
		}{Seq: res}
	}
	return res
}

// FilterMap returns a sequence that only contains transformed items of the specified sequence where the specified function returned true.
func FilterMap[ItemIn, ItemOut any](seq Seq[ItemIn], mapFn func(ItemIn) (ItemOut, bool)) Seq[ItemOut] {
	res := FromIter(iter.FilterMap(ToIter(seq), mapFn))
	if IsInfinite(seq) {
		res = struct {
			Seq[ItemOut]
			canDivergeMark
			infiniteMark
		}{Seq: res}
	}
	return res
}

// Filter returns a sequence of items that only contains items of the specified sequence that match the specified predicate.
// The predicate also receives the ordinal of the item.
func FilterWithIndex[Item any](seq Seq[Item], pred func(int, Item) bool) Seq[Item] {
	res := Map(Filter(Enumerate(seq), func(p Pair[int, Item]) bool { return pred(p.Unpack()) }), second)
	if IsInfinite(seq) {
		res = struct {
			Seq[Item]
			canDivergeMark
			infiniteMark
		}{Seq: res}
	}
	return res
}

// First returns the first item of the specified sequence and true, or the zero value for Item and false if the sequence is empty.
func First[SeqOfItem Seq[Item], Item any](seq SeqOfItem) (first Item, hasFirst bool) {
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		return firstOfFiniteSeq(finiteSeq)
	}
	return iter.First(ToIter(seq))
}

// Flatten returns the concatenation of sequences yielded by the specified sequence.
func Flatten[Seqs Seq[SeqOfItem], SeqOfItem Seq[Item], Item any](seqs Seqs) Seq[Item] {
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seqs); isFiniteSeq {
		return concat(finiteSeq)
	}
	res := FromIter(iter.Flatten(iter.Map(ToIter(seqs), ToIter)))
	if IsInfinite(seqs) {
		res = struct {
			Seq[Item]
			infiniteMark
			canDivergeMark
		}{Seq: res}
	}
	return res
}

// Fold returns the result of successively applying the specified combining function
// to items from the specified sequence, starting with the seed value.
// When the sequence is empty, the result will be the seed value.
//
// Application to a known-to-be-infinite sequence will panic.
// Application to any other infinite sequence will block indefinitely.
func Fold[Item, Result any](seq Seq[Item], seed Result, combine func(Result, Item) Result) Result {
	if IsInfinite(seq) {
		panic(errors.New("infinite sequence cannot be folded"))
	}
	return iter.Fold(ToIter(seq), seed, combine)
}

// Folds returns a sequence of partial results of successively applying the specified combining function
// to items from the specified sequence, starting with the seed value.
func Folds[ItemIn, ItemOut any](seq Seq[ItemIn], seed ItemOut, combine func(ItemOut, ItemIn) ItemOut) Seq[ItemOut] {
	res := FromIter(iter.Folds(ToIter(seq), seed, combine))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, func() int { return 1 + finiteSeq.Len() })
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// FoldsWhile returns a sequence of partial results of successively applying the specified combining function
// to items from the specified sequence while its second return value is true, starting with the seed value.
//
// TL;DR: it's [Folds] with early return.
func FoldsWhile[ItemIn, ItemOut any](seq Seq[ItemIn], seed ItemOut, combine func(ItemOut, ItemIn) (ItemOut, bool)) Seq[ItemOut] {
	return FromIter(iter.FoldsWhile(ToIter(seq), seed, combine))
}

// FoldWhile returns the result of successively applying the specified combining function
// to items from the specified sequence while its second return value is true, starting with the seed value.
// When the sequence is empty, the result will be the seed value.
//
// TL;DR: it's [Fold] with early return.
func FoldWhile[Item, Result any](seq Seq[Item], seed Result, combine func(Result, Item) (Result, bool)) Result {
	return iter.FoldWhile(ToIter(seq), seed, combine)
}

// FromIter returns a sequence from the specified [iter.Seq].
func FromIter[Item any](seq iter.Seq[Item]) Seq[Item] {
	return seqFunc[Item](seq)
}

// FromIter2 returns a sequence from the specified [iter.Seq2].
func FromIter2[ItemIn1, ItemIn2, ItemOut any](seq iter.Seq2[ItemIn1, ItemIn2], pack func(ItemIn1, ItemIn2) ItemOut) Seq[ItemOut] {
	return FromIter(iter.PackMap(seq, pack))
}

// FromValues returns a sequence made up of the specified values.
func FromValues[Item any](values ...Item) Seq[Item] {
	return FromSlice(values)
}

// FromSlice returns a sequence whose items are (copies of) the specified slice's values.
func FromSlice[Slice ~[]Item, Item any](slice Slice) Seq[Item] {
	return sliceSeq[Item](slice)
}

// FromSlicePtrs returns a sequence whose items are pointers to the specified slice's values.
func FromSlicePtrs[Slice ~[]Item, Item any](slice Slice) Seq[*Item] {
	return slicePtrsSeq[Item](slice)
}

// Generate returns a sequence of items obtained by calling the specified function repeatedly.
func Generate[Item any](next func() Item) Seq[Item] {
	return markInfinite(FromIter(iter.Generate(next)))
}

// Generate returns a sequence of items obtained by calling the specified function repeatedly until it returns false.
func GenerateWhile[Item any](next func() (Item, bool)) Seq[Item] {
	return FromIter(iter.GenerateWhile(next))
}

// Inspect returns a sequence whose items are the same as the specified sequence's
// but are passed to the specified function before being yielded.
func Inspect[Item any](seq Seq[Item], observe func(Item)) Seq[Item] {
	res := FromIter(iter.Inspect(ToIter(seq), observe))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, finiteSeq.Len)
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// Interleave returns a sequence of items obtained by cycling between the specified sequences for each item.
// When any of the input sequences is exhausted the sequence ends.
func Interleave[Item any](seqs ...Seq[Item]) Seq[Item] {
	res := FromIter(int_iter.Interleave(wrapSeqSlice(seqs)))
	if lengths, ok := getLengths(wrapSeqSlice(seqs)); ok {
		res = makeFinite(res, func() int {
			/*
				       ○
				     ● ●   ○
				 ↑   ● ● ● ● ●		(yielded: ●, unyielded: ○)
				     ● ● ● ● ●
				     ● ● ● ● ●
				seq  0 1 2 3 4
				         ↑
						min

				min len: 3
				min idx: 2
				    len: 5 * 3 + 2 = 17
			*/
			minIdx, minLen := iter.Reduce2(iter.Enumerate(lengths), func(minIdx int, minLen int, idx int, len int) (int, int) {
				if len < minLen {
					return idx, len
				}
				return minIdx, minLen
			})
			return len(seqs)*minLen + minIdx
		})
	}
	return res
}

// Intersperse returns a sequence of items where separators are inserted between items from the specified sequence.
func Intersperse[Item any](seq Seq[Item], sep Item) Seq[Item] {
	res := FromIter(iter.Intersperse(ToIter(seq), sep))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, func() int { return max(finiteSeq.Len()*2-1, 0) })
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// IsEmpty returns whether the specified sequence has no items.
func IsEmpty[SeqOfItem Seq[Item], Item any](seq SeqOfItem) bool {
	if isEmpty, weKnow := looksEmpty(seq); weKnow {
		return isEmpty
	}
	return iter.IsEmpty(ToIter(seq))
}

// IsFinite returns whether the specified sequence is known to have a finite number of items.
func IsFinite[SeqOfItem Seq[Item], Item any](seq SeqOfItem) bool {
	_, isFinite := AsFiniteSeq(seq)
	return isFinite
}

// IsInfinite returns whether the specified sequence is known to be infinite.
func IsInfinite[SeqOfItem Seq[Item], Item any](seq SeqOfItem) bool {
	_, isInfinite := any(seq).(interface{ Infinite() })
	return isInfinite
}

// Last returns the last item of the specified sequence if it exists.
//
// Known-to-be-infinite sequences are considered not to have a last item.
// Application to any other infinite sequence will block indefinitely.
func Last[SeqOfItem Seq[Item], Item any](seq SeqOfItem) (last Item, hasLast bool) {
	if IsInfinite(seq) {
		return
	}
	if len, hasLen := getLength(seq); hasLen {
		if len < 1 {
			return
		}
		if indexable, isIndexable := asIndexable(seq); isIndexable {
			return indexable.Index(len - 1), true
		}
	}
	return iter.Last(ToIter(seq))
}

// Len returns the length of the specified sequence.
//
// Application to a known-to-be-infinite sequence will panic.
// Application to any other infinite sequence will block indefinitely.
func Len[SeqOfItem Seq[Item], Item any](seq SeqOfItem) int {
	if IsInfinite(seq) {
		panic(errors.New("length of infinite sequence cannot be calculated"))
	}
	if len, hasLen := getLength(seq); hasLen {
		return len
	}
	return iter.Len(ToIter(seq))
}

// Map returns a sequence of items obtained by transforming each item of the specified sequence using the specified function.
func Map[ItemIn, ItemOut any](seq Seq[ItemIn], mapFn func(ItemIn) ItemOut) Seq[ItemOut] {
	res := FromIter(iter.Map(ToIter(seq), mapFn))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, finiteSeq.Len)
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// Map returns a sequence of items obtained by transforming each item of the specified sequence using the specified function.
// The mapping function also receives the ordinal of the item.
func MapWithIndex[ItemIn, ItemOut any](seq Seq[ItemIn], mapFn func(int, ItemIn) ItemOut) Seq[ItemOut] {
	res := FromIter(iter.PackMap(iter.Enumerate(ToIter(seq)), mapFn))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, finiteSeq.Len)
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// Max returns the largest item in the specified sequence.
// It returns the zero value for Item when the sequence is empty.
//
// Application to a known-to-be-infinite sequence will panic.
// Application to any other infinite sequence will block indefinitely.
func Max[SeqOfItem Seq[Item], Item cmp.Ordered](seq SeqOfItem) Item {
	if IsInfinite(seq) {
		panic(errors.New("maximum of infinite sequence cannot be calculated"))
	}
	return iter.Max(ToIter(seq))
}

// Memoize returns a sequence of items that yields memoized items from the specified underlying sequence.
// Each item of the specified sequence will only be forced at most once.
func Memoize[SeqOfItem Seq[Item], Item any](seq SeqOfItem) Seq[Item] {
	// NOTE: We could possibly give a hint to Memoize for how much storage to allocate based on the length of seq but that might turn out to be wasteful when the sequence is only partially enumerated.
	//       Might be worth defining something like MemoizePrealloc though (or add it as an optional param).
	res := FromIter(iter.Memoize(ToIter(seq)))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, finiteSeq.Len)
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// Min returns the smallest item in the specified sequence.
// It returns the zero value for Item when the sequence is empty.
//
// Application to a known-to-be-infinite sequence will panic.
// Application to any other infinite sequence will block indefinitely.
func Min[SeqOfItem Seq[Item], Item cmp.Ordered](seq SeqOfItem) Item {
	if IsInfinite(seq) {
		panic(errors.New("minimum of infinite sequence cannot be calculated"))
	}
	return iter.Min(ToIter(seq))
}

// Or returns the logical OR of the boolean values in the specified sequence.
// The evaluation is short-circuiting.
//
// Application to an infinite sequence might block indefinitely.
func Or[SeqOfBool Seq[bool]](seq SeqOfBool) bool {
	return iter.Or(ToIter(seq))
}

// Panic returns a sequence of items that panics with the specified reason when enumerated.
func Panic[Item any](reason any) Seq[Item] {
	return FromIter(iter.Panic[Item](reason))
}

// Reduce returns the result of successively applying the specified combining function to items from the specified sequence.
// When the sequence is empty, the result will be the zero value for Item.
// When the sequence has a single item, that item will be the result.
//
// Application to a known-to-be-infinite sequence will panic.
// Application to any other infinite sequence will block indefinitely.
func Reduce[Item any](seq Seq[Item], combine func(Item, Item) Item) Item {
	if IsInfinite(seq) {
		panic(errors.New("infinite sequence cannot be reduced"))
	}
	return iter.Reduce(ToIter(seq), combine)
}

// Reductions returns a sequence of partial results of successively applying the specified combining function to items from the specified sequence.
// The first item of the returned sequence will be the first item of the specified sequence.
// When the specified sequence is empty, the returned sequence will be empty.
func Reductions[Item any](seq Seq[Item], combine func(Item, Item) Item) Seq[Item] {
	res := FromIter(iter.Reductions(ToIter(seq), combine))

	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, finiteSeq.Len) // same length as seq
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// ReduceWhile returns the result of successively applying the specified combining function
// to items from the specified sequence while its second return value is true.
// When the sequence is empty, the result will be the zero value for Item.
// When the sequence has a single item, that item will be the result.
//
// TL;DR: it's [Reduce] with early return.
func ReduceWhile[Item any](seq Seq[Item], combine func(Item, Item) (Item, bool)) Item {
	return iter.ReduceWhile(ToIter(seq), combine)
}

// ReductionsWhile returns a sequence of partial results of successively applying the specified combining function
// to items from the specified sequence while its second return value is true.
// The first item of the returned sequence will be the first item of the specified sequence.
// When the specified sequence is empty, the returned sequence will be empty.
//
// TL;DR: it's [Reductions] with early return.
func ReductionsWhile[Item any](seq Seq[Item], combine func(Item, Item) (Item, bool)) Seq[Item] {
	return FromIter(iter.ReductionsWhile(ToIter(seq), combine))
}

// Repeat returns an infinite sequence that repeats the specified value
func Repeat[Item any](item Item) Seq[Item] {
	return markInfinite(FromIter(iter.Repeat(item)))
}

// Repeat returns a finite sequence that repeats the specified value `n` times
func RepeatN[Item any](item Item, n int) Seq[Item] {
	return makeFinite(
		FromIter(iter.RepeatN(item, n)),
		func() int { return n },
	)
}

// Singleton returns a singleton sequence containing the specified item.
func Singleton[Item any](item Item) Seq[Item] {
	return singleton[Item]{item: item}
}

// Sum returns the sum of items in the specified sequence.
//
// Application to a known-to-be-infinite sequence will panic.
// Application to any other infinite sequence will block indefinitely.
func Sum[SeqOfItem Seq[Item], Item Summable](seq SeqOfItem) Item {
	if IsInfinite(seq) {
		panic(errors.New("infinite sequence cannot be summed"))
	}
	return iter.Sum(ToIter(seq))
}

// Sums returns a sequence of partial sums of items in the specified sequence.
func Sums[SeqOfItem Seq[Item], Item Summable](seq SeqOfItem) Seq[Item] {
	res := FromIter(iter.Sums(ToIter(seq)))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, finiteSeq.Len)
	} else if IsInfinite(seq) {
		res = markInfinite(res)
	}
	return res
}

// Take returns a sequence of at most n items from the start of the specified sequence.
func Take[Item any](seq Seq[Item], n int) Seq[Item] {
	if n == 0 {
		return Empty[Item]()
	}

	res := FromIter(iter.Take(ToIter(seq), n))
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		res = makeFinite(res, func() int { return min(finiteSeq.Len(), n) })
	} else if IsInfinite(seq) {
		res = makeFinite(res, func() int { return n })
	}
	return res
}

// TakeWhile returns a prefix of the specified sequence that contains only items that match the specified predicate.
func TakeWhile[Item any](seq Seq[Item], pred Pred[Item]) Seq[Item] {
	return FromIter(iter.TakeWhile(ToIter(seq), pred))
}

// ToIter returns the sequence as an [iter.Seq]
func ToIter[SeqOfItem Seq[Item], Item any](seq SeqOfItem) iter.Seq[Item] {
	return seq.ForEachWhile
}

// ToSlice returns a slice of the specified sequence's items
func ToSlice[SeqOfItem Seq[Item], Item any](seq SeqOfItem) (slice []Item) {
	if length, ok := getLength(seq); ok && length > 0 {
		slice = make([]Item, 0, length)
	}
	return slices.AppendSeq(slice, ToIter(seq))
}

// Unfold returns a sequence of items generated by successively applying the specified function to the seed value.
//
// The specified function should return three values:
// * the next item when there is one OR the zero value of [Item] when there isn't
// * whether there is a next item
// * the seed value for the next invocation of the function.
func Unfold[Item, State any](seed State, next func(State) (Item, bool, State)) Seq[Item] {
	return FromIter(iter.Unfold(seed, next))
}

// YieldAll yields all items from the specified sequence using the specified function.
// It returns false if yield returned false.
//
// This can be useful when forwarding enumeration to a child sequence or emulating Python's for-else.
func YieldAll[Item any](seq Seq[Item], yield func(Item) bool) bool {
	return iter.YieldAll(ToIter(seq), yield)
}

// ZipMany returns a sequence of slices obtained by taking corresponding items from the specified sequences.
func ZipMany[Item any](seqs ...Seq[Item]) Seq[[]Item] {
	switch len(seqs) {
	case 0:
		return Empty[[]Item]()
	case 1:
		return Map(seqs[0], func(item Item) []Item { return []Item{item} })
	}

	res := FromIter(int_iter.ZipMany(wrapSeqSlice(seqs)))
	if lengths, ok := getLengths(wrapSeqSlice(seqs)); ok {
		res = makeFinite(res, func() int { return iter.Min(lengths) })
	}
	return res
}

// ZipWith returns a sequence containing results of applying the specified combining function to corresponding items from the specified sequences.
func ZipWith[ItemIn1, ItemIn2, ItemOut any](seq1 Seq[ItemIn1], seq2 Seq[ItemIn2], combine func(ItemIn1, ItemIn2) ItemOut) Seq[ItemOut] {
	res := FromIter(iter.PackMap(iter.Zip(ToIter(seq1), ToIter(seq2)), combine))
	finiteSeq1, isFiniteSeq1 := AsFiniteSeq(seq1)
	finiteSeq2, isFiniteSeq2 := AsFiniteSeq(seq2)
	if isFiniteSeq1 && isFiniteSeq2 {
		res = makeFinite(res, func() int { return min(finiteSeq1.Len(), finiteSeq2.Len()) })
	}
	return res
}

func asIndexable[SeqOfItem Seq[Item], Item any](seq SeqOfItem) (indexable Indexable[Item], isIndexable bool) {
	indexable, isIndexable = any(seq).(Indexable[Item])
	return
}

// canDivergeMark is a marker type for seqs that are known to be possibly divergent (e.g. Filter(Repeat(0), func(i int) bool { return i > 0 }))
type canDivergeMark struct{}

func (canDivergeMark) CanDiverge() {}

func concat[
	Seqs FiniteSeq[SeqOfItem],
	SeqOfItem Seq[Item],
	Item any,
](seqs Seqs) Seq[Item] {
	switch seqs.Len() {
	case 0:
		return Empty[Item]()
	case 1:
		seq, _ := firstOfFiniteSeq(seqs)
		return seq
	}

	res := FromIter(iter.Flatten(iter.Map(ToIter(seqs), ToIter)))
	if lengths, ok := getLengths(seqs); ok {
		res = makeFinite(res, func() int { return iter.Sum(lengths) })
	} else if Any(seqs, IsInfinite) {
		res = markInfinite(res)
	}
	return res
}

type emptySeq[Item any] struct{}

func (emptySeq[Item]) ForEachWhile(func(Item) bool) {}

func (emptySeq[_]) Len() int { return 0 }

func makeFinite[Item any](seq Seq[Item], lenFn lenFunc) FiniteSeq[Item] {
	return finiteSeq[Item]{
		Seq:   seq,
		lenFn: lenFn,
	}
}

type finiteSeq[Item any] struct {
	Seq[Item]
	lenFn lenFunc
}

func (seq finiteSeq[Item]) Len() int {
	return seq.lenFn()
}

func markInfinite[Item any](seq Seq[Item]) Seq[Item] {
	return struct {
		Seq[Item]
		infiniteMark
	}{
		Seq: seq,
	}
}

type infiniteMark struct{}

func (infiniteMark) Infinite() {}

type lenFunc = func() int

type seqFunc[Item any] iter.Seq[Item]

func (seq seqFunc[Item]) ForEachWhile(yield func(Item) bool) {
	seq(yield)
}

type singleton[Item any] struct {
	item Item
}

func (s singleton[Item]) ForEachWhile(yield func(Item) bool) {
	_ = yield(s.item)
}

func (seq singleton[Item]) Index(idx int) Item {
	return [...]Item{seq.item}[idx]
}

func (singleton[_]) Len() int {
	return 1
}

type sliceSeq[Item any] []Item

func (seq sliceSeq[Item]) ForEachWhile(yield func(Item) bool) {
	slices.Values(seq)(yield)
}

func (seq sliceSeq[Item]) Index(idx int) Item {
	return seq[idx]
}

func (seq sliceSeq[_]) Len() int {
	return len(seq)
}

type slicePtrsSeq[Item any] []Item

func (seq slicePtrsSeq[Item]) ForEachWhile(yield func(*Item) bool) {
	for i := range seq {
		if !yield(&seq[i]) {
			return
		}
	}
}

func (seq slicePtrsSeq[Item]) Index(idx int) *Item {
	return &seq[idx]
}

func (seq slicePtrsSeq[_]) Len() int {
	return len(seq)
}

func firstOfFiniteSeq[Item any](seq FiniteSeq[Item]) (Item, bool) {
	if seq.Len() == 0 {
		return *new(Item), false
	}
	if indexable, isIndexable := asIndexable(seq); isIndexable {
		return indexable.Index(0), true
	}
	return iter.First(ToIter(seq))
}

func getLength[Item any](seq Seq[Item]) (int, bool) {
	if finiteSeq, isFiniteSeq := AsFiniteSeq(seq); isFiniteSeq {
		return finiteSeq.Len(), true
	}
	return 0, false
}

func getLengths[
	Seqs FiniteSeq[SeqOfItem],
	SeqOfItem Seq[Item],
	Item any,
](seqs Seqs) (iter.Seq[int], bool) {
	for seq := range ToIter(seqs) {
		if !IsFinite(seq) {
			return nil, false
		}
	}
	return iter.Map(ToIter(seqs), func(seq SeqOfItem) int {
		return any(seq).(FiniteSeq[Item]).Len()
	}), true
}

func looksEmpty[Item any](seq Seq[Item]) (isEmpty bool, weKnow bool) {
	if len, hasLen := getLength(seq); hasLen {
		return len == 0, true
	}
	if IsInfinite(seq) {
		return false, true
	}
	return false, false
}

func roundDownDiv(nom int, denom int) int {
	return nom / denom
}

func roundUpDiv(nom int, denom int) int {
	return (nom + denom - 1) / denom
}

func wrapSeqSlice[SeqOfItem Seq[Item], Item any](seqs []SeqOfItem) seqSlice[SeqOfItem, Item] {
	return seqSlice[SeqOfItem, Item](seqs)
}

type seqSlice[SeqOfItem Seq[Item], Item any] []SeqOfItem

func (seqs seqSlice[SeqOfItem, _]) ForEachWhile(yield func(SeqOfItem) bool) {
	slices.Values(seqs)(yield)
}

func (seqs seqSlice[SeqOfItem, _]) Index(idx int) SeqOfItem {
	return seqs[idx]
}

func (seqs seqSlice[SeqOfItem, Item]) ItemsWithIndex(yield func(int, iter.Seq[Item]) bool) {
	iter.Map2(slices.All(seqs), func(i int, v SeqOfItem) (int, iter.Seq[Item]) { return i, ToIter(v) })(yield)
}

func (seqs seqSlice[_, _]) Len() int {
	return len(seqs)
}

func pairFrom[Value1, Value2 any](value1 Value1, value2 Value2) Pair[Value1, Value2] {
	return internal.PairFrom(value1, value2)
}

func second[P Pair[Value1, Value2], Value1, Value2 any](pair P) Value2 {
	_, value2 := pair.Unpack()
	return value2
}
