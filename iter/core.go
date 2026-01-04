package iter

import (
	"cmp"
	"slices"
	"sync"
	"sync/atomic"

	"github.com/siliconbrain/go-seqs/internal"
	int_iter "github.com/siliconbrain/go-seqs/internal/iter"
)

// All returns whether the specified predicate matches all items in the specified sequence.
func All[Item any](seq Seq[Item], pred Pred[Item]) bool {
	return And(Map(seq, pred))
}

// All2 returns whether the specified predicate matches all pairs in the specified sequence.
func All2[Item1, Item2 any](seq Seq2[Item1, Item2], pred Pred2[Item1, Item2]) bool {
	return And(PackMap(seq, pred))
}

// And returns the logical AND of the boolean values in the specified sequence.
// The evaluation is short-circuiting.
func And(seq Seq[bool]) bool {
	for v := range seq {
		if !v {
			return false
		}
	}
	return true
}

// Any returns whether the specified predicate matches any items in the specified sequence.
func Any[Item any](seq Seq[Item], pred Pred[Item]) bool {
	return Or(Map(seq, pred))
}

func Any2[Item1, Item2 any](seq Seq2[Item1, Item2], pred Pred2[Item1, Item2]) bool {
	return Or(PackMap(seq, pred))
}

// Bimap returns a sequence of pairs where each value in the pair is the result of applying the two supplied functions to the respective values of a pair from the specified sequence.
func Bimap[ItemIn1, ItemIn2, ItemOut1, ItemOut2 any](
	seq Seq2[ItemIn1, ItemIn2], mapFn1 func(ItemIn1) ItemOut1, mapFn2 func(ItemIn2) ItemOut2,
) Seq2[ItemOut1, ItemOut2] {
	return func(yield func(ItemOut1, ItemOut2) bool) {
		for item1, item2 := range seq {
			if !yield(mapFn1(item1), mapFn2(item2)) {
				return
			}
		}
	}
}

// Cartesian returns a sequence of pairs where each pair is a member of the cartesian product of
// (i.e. all combinations of items from) the two specified sequences.
func Cartesian[Item1, Item2 any](seq1 Seq[Item1], seq2 Seq[Item2]) Seq2[Item1, Item2] {
	return Flatten2(Map(seq1, func(item1 Item1) Seq2[Item1, Item2] {
		return UnpackMap(seq2, func(item2 Item2) (Item1, Item2) {
			return item1, item2
		})
	}))
}

// Concat returns a sequence of items that is the concatenation of items from the specified sequences.
func Concat[Item any](seqs ...Seq[Item]) Seq[Item] {
	return Flatten(slices.Values(seqs))
}

// Concat2 returns a sequence of pairs that is the concatenation of pairs from the specified sequences.
func Concat2[Item1, Item2 any](seqs ...Seq2[Item1, Item2]) Seq2[Item1, Item2] {
	return Flatten2(slices.Values(seqs))
}

// Count returns a sequence of repeatedly adding step to the previous value, starting with from.
func Count[Item Summable](from Item, step Item) Seq[Item] {
	return Unfold(from, func(v Item) (Item, bool, Item) { return v, true, v + step })
}

// Cycle returns a sequence of items that infinitely repeates the specified sequence.
func Cycle[Item any](seq Seq[Item]) Seq[Item] {
	return int_iter.Cycle(seq)
}

// Cycle2 returns a sequence of pairs that infinitely repeates the specified sequence.
func Cycle2[Item1, Item2 any](seq Seq2[Item1, Item2]) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		for {
			empty := true
			for item1, item2 := range seq {
				empty = false
				if !yield(item1, item2) {
					return
				}
			}
			if empty {
				return
			}
		}
	}
}

// DemuxMap2 turns a sequence of homogeneous (in type) items into a sequence of 2-way heterogeneous items by using the specified demultiplexing function.
func DemuxMap2[ItemIn, ItemOut1, ItemOut2 any](seq Seq[ItemIn], demux func(item ItemIn, k1 func(ItemOut1), k2 func(ItemOut2))) MuxSeq2[ItemOut1, ItemOut2] {
	return func(yield1 func(ItemOut1) bool, yield2 func(ItemOut2) bool) {
		seq(func(item ItemIn) bool {
			cont := false
			demux(item,
				func(item ItemOut1) { cont = yield1(item) },
				func(item ItemOut2) { cont = yield2(item) },
			)
			return cont
		})
	}
}

// DemuxMap3 turns a sequence of homogeneous (in type) items into a sequence of 3-way heterogeneous items by using the specified demultiplexing function.
func DemuxMap3[ItemIn, ItemOut1, ItemOut2, ItemOut3 any](seq Seq[ItemIn], demux func(item ItemIn, k1 func(ItemOut1), k2 func(ItemOut2), k3 func(ItemOut3))) MuxSeq3[ItemOut1, ItemOut2, ItemOut3] {
	return func(yield1 func(ItemOut1) bool, yield2 func(ItemOut2) bool, yield3 func(ItemOut3) bool) {
		seq(func(item ItemIn) bool {
			cont := false
			demux(item,
				func(item ItemOut1) { cont = yield1(item) },
				func(item ItemOut2) { cont = yield2(item) },
				func(item ItemOut3) { cont = yield3(item) },
			)
			return cont
		})
	}
}

// DemuxMap4 turns a sequence of homogeneous (in type) items into a sequence of 4-way heterogeneous items by using the specified demultiplexing function.
func DemuxMap4[ItemIn, ItemOut1, ItemOut2, ItemOut3, ItemOut4 any](seq Seq[ItemIn], demux func(item ItemIn, k1 func(ItemOut1), k2 func(ItemOut2), k3 func(ItemOut3), k4 func(ItemOut4))) MuxSeq4[ItemOut1, ItemOut2, ItemOut3, ItemOut4] {
	return func(yield1 func(ItemOut1) bool, yield2 func(ItemOut2) bool, yield3 func(ItemOut3) bool, yield4 func(ItemOut4) bool) {
		seq(func(item ItemIn) bool {
			cont := false
			demux(item,
				func(item ItemOut1) { cont = yield1(item) },
				func(item ItemOut2) { cont = yield2(item) },
				func(item ItemOut3) { cont = yield3(item) },
				func(item ItemOut4) { cont = yield4(item) },
			)
			return cont
		})
	}
}

// Divvy returns a sequence of slices with at most size length containing a continuous range of items from the specified sequence.
// The start of each slice will be offset by skip number of items from the previous one.
// Slices will overlap when size > skip, and some items will be dropped when size < skip.
func Divvy[Item any](seq Seq[Item], size int, skip int) Seq[[]Item] {
	if size < 1 {
		panic("size must be positive")
	}
	if skip < 1 {
		panic("skip must be positive")
	}
	shift := min(skip, size)
	ignore := skip - shift
	return func(yield func([]Item) bool) {
		buf := make([]Item, 0, size)
		ignoreCnt := 0
		shouldFlush := false
		for item := range seq {
			if ignoreCnt > 0 {
				ignoreCnt--
				continue
			}
			buf = append(buf, item)
			shouldFlush = true
			if len(buf) == size {
				if !yield(slices.Clone(buf)) {
					return
				}
				buf = append(buf[0:0], buf[shift:]...)
				ignoreCnt = ignore
				shouldFlush = false
			}
		}
		if shouldFlush {
			_ = yield(buf)
		}
	}
}

// DivvyExact is like [Divvy] but all yielded slices are exactly size length.
// Any trailing items are dropped.
func DivvyExact[Item any](seq Seq[Item], size int, skip int) Seq[[]Item] {
	if size < 1 {
		panic("size must be positive")
	}
	if skip < 1 {
		panic("skip must be positive")
	}
	shift := min(skip, size)
	ignore := skip - shift
	return func(yield func([]Item) bool) {
		buf := make([]Item, 0, size)
		ignoreCnt := 0
		for item := range seq {
			if ignoreCnt > 0 {
				ignoreCnt--
				continue
			}
			buf = append(buf, item)
			if len(buf) == size {
				if !yield(slices.Clone(buf)) {
					return
				}
				buf = append(buf[0:0], buf[shift:]...)
				ignoreCnt = ignore
			}
		}
	}
}

// Drop returns a sequence with at most n items dropped from the start of the specified sequence.
func Drop[Item any](seq Seq[Item], n int) Seq[Item] {
	if n <= 0 {
		return seq
	}

	return func(yield func(Item) bool) {
		i := 0
		for item := range seq {
			if i < n {
				i++
				continue
			}
			if !yield(item) {
				return
			}
		}
	}
}

// Drop2 returns a sequence with at most n pairs dropped from the start of the specified sequence.
func Drop2[Item1, Item2 any](seq Seq2[Item1, Item2], n int) Seq2[Item1, Item2] {
	if n <= 0 {
		return seq
	}

	return func(yield func(Item1, Item2) bool) {
		i := 0
		for item1, item2 := range seq {
			if i < n {
				i++
				continue
			}
			if !yield(item1, item2) {
				return
			}
		}
	}
}

// DropLast returns the specified sequence of items without its last n items.
// When the specified sequence has less than n items, the result will be empty.
//
// Uses O(n) space.
func DropLast[Item any](seq Seq[Item], n int) Seq[Item] {
	if n <= 0 {
		return seq
	}
	if n == 1 {
		return func(yield func(Item) bool) {
			next, stop := Pull(seq)
			defer stop()

			lastItem, ok := next()
			if !ok {
				return
			}
			for {
				nextItem, hasNextItem := next()
				if !hasNextItem || !yield(lastItem) {
					return
				}
				lastItem = nextItem
			}
		}
	}

	return func(yield func(Item) bool) {
		dropList := make([]Item, 0, n)
		next, stop := Pull(seq)
		defer stop()

		for len(dropList) < cap(dropList) {
			nextItem, hasNextItem := next()
			if !hasNextItem {
				return
			}
			dropList = append(dropList, nextItem)
		}

		cur := 0
		for {
			nextItem, hasNextItem := next()
			if !hasNextItem || !yield(dropList[cur]) {
				return
			}
			dropList[cur] = nextItem
			cur = (cur + 1) % cap(dropList)
		}
	}
}

// DropLast2 returns the specified sequence of pairs without its last n pairs.
// When the specified sequence has less than n pairs, the result will be empty.
//
// Uses O(n) space.
func DropLast2[Item1, Item2 any](seq Seq2[Item1, Item2], n int) Seq2[Item1, Item2] {
	return UnpackMap(DropLast(PackMap(seq, internal.PairFrom), n), internal.Pair[Item1, Item2].Unpack)
}

// DropWhile returns the rest of the specified sequence after the prefix of items matching the specified predicate.
func DropWhile[Item any](seq Seq[Item], pred Pred[Item]) Seq[Item] {
	return func(yield func(Item) bool) {
		skipping := true
		for item := range seq {
			skipping = skipping && pred(item)
			if skipping {
				continue
			}
			if !yield(item) {
				return
			}
		}
	}
}

// DropWhile2 returns the rest of the specified sequence after the prefix of pairs matching the specified predicate.
func DropWhile2[Item1, Item2 any](seq Seq2[Item1, Item2], pred Pred2[Item1, Item2]) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		skipping := true
		for item1, item2 := range seq {
			skipping = skipping && pred(item1, item2)
			if skipping {
				continue
			}
			if !yield(item1, item2) {
				return
			}
		}
	}
}

// Empty is an empty sequence of items.
func Empty[Item any](yield func(Item) bool) {}

// Empty2 is an empty sequence of pairs.
func Empty2[Item1, Item2 any](yield func(Item1, Item2) bool) {}

// EmptyMux2 is an empty sequence of 2-way heterogeneous items.
func EmptyMux2[Item1, Item2 any](yield1 func(Item1) bool, yield2 func(Item2) bool) {}

// EmptyMux3 is an empty sequence of 3-way heterogeneous items.
func EmptyMux3[Item1, Item2, Item3 any](yield1 func(Item1) bool, yield2 func(Item2) bool, yield3 func(Item3) bool) {
}

// EmptyMux4 is an empty sequence of 4-way heterogeneous items.
func EmptyMux4[Item1, Item2, Item3, Item4 any](yield1 func(Item1) bool, yield2 func(Item2) bool, yield3 func(Item3) bool, yield4 func(Item4) bool) {
}

// Enumerate returns a sequence of pairs where each pair consists of the ordinal of an item from the specified sequence and the item itself.
func Enumerate[Item any](seq Seq[Item]) Seq2[int, Item] {
	return Zip(Count(0, 1), seq)
}

// Filter returns a sequence of items that only contains items of the specified sequence that match the specified predicate.
func Filter[Item any](seq Seq[Item], pred Pred[Item]) Seq[Item] {
	return func(yield func(Item) bool) {
		for item := range seq {
			if pred(item) {
				if !yield(item) {
					return
				}
			}
		}
	}
}

// Filter2 returns a sequence of pairs that only contains pairs of the specified sequence that match the specified predicate.
func Filter2[Item1, Item2 any](seq Seq2[Item1, Item2], pred Pred2[Item1, Item2]) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		for item1, item2 := range seq {
			if pred(item1, item2) {
				if !yield(item1, item2) {
					return
				}
			}
		}
	}
}

// FilterMap returns a sequence that only contains transformed items of the specified sequence where the specified function returned true.
func FilterMap[ItemIn, ItemOut any](seq Seq[ItemIn], mapFn func(ItemIn) (ItemOut, bool)) Seq[ItemOut] {
	return PackMap(Filter2(UnpackMap(seq, mapFn), second), first)
}

// FilterMap2 returns a sequence that only contains transformed pairs of the specified sequence where the specified function returned true.
func FilterMap2[ItemIn1, ItemIn2, ItemOut1, ItemOut2 any](
	seq Seq2[ItemIn1, ItemIn2], mapFn func(ItemIn1, ItemIn2) (ItemOut1, ItemOut2, bool),
) Seq2[ItemOut1, ItemOut2] {
	return func(yield func(ItemOut1, ItemOut2) bool) {
		for item1, item2 := range seq {
			if out1, out2, ok := mapFn(item1, item2); ok {
				if !yield(out1, out2) {
					return
				}
			}
		}
	}
}

// First returns the first item of the specified sequence and true, or the zero value for [Item] and false if the sequence is empty.
func First[Item any](seq Seq[Item]) (Item, bool) {
	for item := range seq {
		return item, true
	}
	return *new(Item), false
}

// First2 returns the first pair of the specified sequence and true, or the zero value for [Item1] and [Item2], and false if the sequence is empty.
func First2[Item1, Item2 any](seq Seq2[Item1, Item2]) (Item1, Item2, bool) {
	for item1, item2 := range seq {
		return item1, item2, true
	}
	return *new(Item1), *new(Item2), false
}

// Flatten returns the concatenation of sequences yielded by the specified sequence.
func Flatten[Item any](seqs Seq[Seq[Item]]) Seq[Item] {
	return func(yield func(Item) bool) {
		for seq := range seqs {
			if !YieldAll(seq, yield) {
				return
			}
		}
	}
}

// Flatten2 returns the concatenation of sequences inside the specified sequence.
func Flatten2[Item1, Item2 any](seqs Seq[Seq2[Item1, Item2]]) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		for seq := range seqs {
			if !YieldAll2(seq, yield) {
				return
			}
		}
	}
}

// Fold returns the result of successively applying the specified combining function
// to items from the specified sequence, starting with the seed value.
// When the sequence is empty, the result will be the seed value.
func Fold[Item, Result any](seq Seq[Item], seed Result, combine func(Result, Item) Result) Result {
	res, _ := Last(Folds(seq, seed, combine))
	return res
}

// Fold2 returns the result of successively applying the specified combining function
// to pairs from the specified sequence, starting with the seed value.
// When the sequence is empty, the result will be the seed value.
func Fold2[Item1, Item2, Result any](seq Seq2[Item1, Item2], seed Result, combine func(Result, Item1, Item2) Result) Result {
	res, _ := Last(Folds2(seq, seed, combine))
	return res
}

// Folds returns a sequence of partial results of successively applying the specified combining function
// to items from the specified sequence, starting with the seed value.
func Folds[Item, Result any](seq Seq[Item], seed Result, combine func(Result, Item) Result) Seq[Result] {
	return func(yield func(Result) bool) {
		res := seed
		if !yield(res) {
			return
		}
		for item := range seq {
			res = combine(res, item)
			if !yield(res) {
				return
			}
		}
	}
}

// Folds2 returns a sequence of partial results of successively applying the specified combining function
// to pairs from the specified sequence, starting with the seed value.
func Folds2[Item1, Item2, Result any](seq Seq2[Item1, Item2], seed Result, combine func(Result, Item1, Item2) Result) Seq[Result] {
	return Folds(PackMap(seq, internal.PairFrom), seed, func(r Result, i internal.Pair[Item1, Item2]) Result {
		return combine(r, i.Value1, i.Value2)
	})
}

// FoldsWhile returns a sequence of partial results of successively applying the specified combining function
// to items from the specified sequence while its second return value is true, starting with the seed value.
//
// TL;DR: it's [Folds] with early return.
func FoldsWhile[Item, Result any](seq Seq[Item], seed Result, combine func(Result, Item) (Result, bool)) Seq[Result] {
	return func(yield func(Result) bool) {
		res := seed
		if !yield(res) {
			return
		}
		for item := range seq {
			var ok bool
			res, ok = combine(res, item)
			if !ok || !yield(res) {
				return
			}
		}
	}
}

// FoldsWhile2 returns a sequence of partial results of successively applying the specified combining function
// to pairs from the specified sequence while its second return value is true, starting with the seed value.
//
// TL;DR: it's [Folds2] with early return.
func FoldsWhile2[Item1, Item2, Result any](seq Seq2[Item1, Item2], seed Result, combine func(Result, Item1, Item2) (Result, bool)) Seq[Result] {
	return FoldsWhile(PackMap(seq, internal.PairFrom), seed, func(r Result, i internal.Pair[Item1, Item2]) (Result, bool) {
		return combine(r, i.Value1, i.Value2)
	})
}

// FoldWhile returns the result of successively applying the specified combining function
// to items from the specified sequence while its second return value is true, starting with the seed value.
// When the sequence is empty, the result will be the seed value.
//
// TL;DR: it's [Fold] with early return.
func FoldWhile[Item, Result any](seq Seq[Item], seed Result, combine func(Result, Item) (Result, bool)) Result {
	res, _ := Last(FoldsWhile(seq, seed, combine))
	return res
}

// FoldWhile2 returns the result of successively applying the specified combining function
// to pairs from the specified sequence while its second return value is true, starting with the seed value.
// When the sequence is empty, the result will be the seed value.
//
// TL;DR: it's [Fold2] with early return.
func FoldWhile2[Item1, Item2, Result any](seq Seq2[Item1, Item2], seed Result, combine func(Result, Item1, Item2) (Result, bool)) Result {
	res, _ := Last(FoldsWhile2(seq, seed, combine))
	return res
}

// FromValues returns a sequence that yields the specified values.
func FromValues[Item any](values ...Item) Seq[Item] {
	return slices.Values(values)
}

// Generate returns a sequence of items obtained by calling the specified function repeatedly.
func Generate[Item any](next func() Item) Seq[Item] {
	return func(yield func(Item) bool) {
		for {
			if !yield(next()) {
				return
			}
		}
	}
}

// Generate returns a sequence of pairs obtained by calling the specified function repeatedly.
func Generate2[Item1, Item2 any](next func() (Item1, Item2)) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		for {
			if !yield(next()) {
				return
			}
		}
	}
}

// Generate returns a sequence of items obtained by calling the specified function repeatedly until it returns false.
//
// The item returned with false will not be yielded.
func GenerateWhile[Item any](next func() (Item, bool)) Seq[Item] {
	return func(yield func(Item) bool) {
		for {
			nextItem, hasNextItem := next()
			if !hasNextItem || !yield(nextItem) {
				return
			}
		}
	}
}

// Generate returns a sequence of pairs obtained by calling the specified function repeatedly until it returns false.
//
// The pair returned with false will not be yielded.
func GenerateWhile2[Item1, Item2 any](next func() (Item1, Item2, bool)) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		for {
			nextItem1, nextItem2, hasNextItems := next()
			if !hasNextItems || !yield(nextItem1, nextItem2) {
				return
			}
		}
	}
}

// Inspect returns a sequence whose items are the same as the specified sequence's
// but are passed to the specified function before being yielded.
func Inspect[Item any](seq Seq[Item], observe func(Item)) Seq[Item] {
	return func(yield func(Item) bool) {
		seq(func(item Item) bool {
			observe(item)
			return yield(item)
		})
	}
}

// Inspect2 returns a sequence whose pairs are the same as the specified sequence's
// but are passed to the specified function before being yielded.
func Inspect2[Item1, Item2 any](seq Seq2[Item1, Item2], observe func(Item1, Item2)) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		seq(func(item1 Item1, item2 Item2) bool {
			observe(item1, item2)
			return yield(item1, item2)
		})
	}
}

// Interleave returns a sequence of items obtained by cycling between the specified sequences for each item.
// When any of the input sequences is exhausted the sequence ends.
func Interleave[Item any](seqs ...Seq[Item]) Seq[Item] {
	return func(yield func(Item) bool) {
		nexts := make([]func() (Item, bool), len(seqs))
		for i := range seqs {
			var stop func()
			nexts[i], stop = Pull(seqs[i])
			defer stop()
		}
		for next := range Cycle(slices.Values(nexts)) {
			item, ok := next()
			if !ok || !yield(item) {
				return
			}
		}
	}
}

// Interleave returns a sequence of pairs obtained by cycling between the specified sequences for each pair.
// When any of the input sequences is exhausted the sequence ends.
func Interleave2[Item1, Item2 any](seqs ...Seq2[Item1, Item2]) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		nexts := make([]func() (Item1, Item2, bool), len(seqs))
		for i := range seqs {
			var stop func()
			nexts[i], stop = Pull2(seqs[i])
			defer stop()
		}
		for next := range Cycle(slices.Values(nexts)) {
			item1, item2, ok := next()
			if !ok || !yield(item1, item2) {
				return
			}
		}
	}
}

// Intersperse returns a sequence of items where separators are inserted between items from the specified sequence.
func Intersperse[Item any](seq Seq[Item], sep Item) Seq[Item] {
	return DropLast(Interleave(seq, Repeat(sep)), 1)
}

// Intersperse2 returns a sequence of pairs where separators are inserted between pairs from the specified sequence.
func Intersperse2[Item1, Item2 any](seq Seq2[Item1, Item2], sep1 Item1, sep2 Item2) Seq2[Item1, Item2] {
	return DropLast2(Interleave2(seq, Repeat2(sep1, sep2)), 1)
}

// IsEmpty returns whether the specified sequence has no items.
func IsEmpty[Item any](seq Seq[Item]) bool {
	_, hasFirst := First(seq)
	return !hasFirst
}

// IsEmpty2 returns whether the specified sequence has no pairs.
func IsEmpty2[Item1, Item2 any](seq Seq2[Item1, Item2]) bool {
	_, _, hasFirst := First2(seq)
	return !hasFirst
}

// Last returns the last item of the specified non-empty sequence and true.
// When the specified sequence is empty, it returns the zero value for [Item] and false.
func Last[Item any](seq Seq[Item]) (last Item, hasLast bool) {
	for item := range seq {
		last, hasLast = item, true
	}
	return
}

// Last2 returns the last pair of the specified non-empty sequence and true.
// When the specified sequence is empty, it return the zero values for [Item1] and [Item2], and false.
func Last2[Item1, Item2 any](seq Seq2[Item1, Item2]) (last1 Item1, last2 Item2, hasLast bool) {
	for item1, item2 := range seq {
		last1, last2, hasLast = item1, item2, true
	}
	return
}

// Len returns the length of the specified sequence by counting its items.
func Len[Item any](seq Seq[Item]) (cnt int) {
	for range seq {
		cnt++
	}
	return
}

// Len2 returns the length of the sequence by counting its pairs.
func Len2[Item1, Item2 any](seq Seq2[Item1, Item2]) (cnt int) {
	for range seq {
		cnt++
	}
	return
}

// Map returns a sequence of items obtained by transforming each item of the specified sequence using the specified function.
func Map[ItemIn, ItemOut any](seq Seq[ItemIn], mapFn func(ItemIn) ItemOut) Seq[ItemOut] {
	return func(yield func(ItemOut) bool) {
		for item := range seq {
			if !yield(mapFn(item)) {
				return
			}
		}
	}
}

// Map returns a sequence of pairs obtained by transforming each pair of the specified sequence using the specified function.
func Map2[ItemIn1, ItemIn2, ItemOut1, ItemOut2 any](
	seq Seq2[ItemIn1, ItemIn2], mapFn func(ItemIn1, ItemIn2) (ItemOut1, ItemOut2),
) Seq2[ItemOut1, ItemOut2] {
	return func(yield func(ItemOut1, ItemOut2) bool) {
		for item1, item2 := range seq {
			if !yield(mapFn(item1, item2)) {
				return
			}
		}
	}
}

// Max returns the largest item in the specified sequence.
// It returns the zero value for Item when the sequence is empty.
func Max[Item cmp.Ordered](seq Seq[Item]) Item {
	return Reduce(seq, func(item1, item2 Item) Item { return max(item1, item2) })
}

// Memoize returns a sequence of items that yields memoized items from the specified underlying sequence.
// Each item of the specified sequence will only be forced at most once.
func Memoize[Item any](seq Seq[Item]) Seq[Item] {
	var cache []Item
	var complete atomic.Bool
	var lock sync.Mutex

	next, _ := Pull(seq)

	getMore := func(offset int) []Item {
		lock.Lock()
		defer lock.Unlock()

		for len(safeSuffix(cache, offset)) == 0 {
			item, ok := next()
			if !ok { // no more items in seq, cache complete
				complete.Store(true)
				break
			}
			cache = append(cache, item)
		}

		return safeSuffix(cache, offset)
	}

	return func(yield func(Item) bool) {
		if complete.Load() { // all items from seq are in the cache, no sync required
			slices.Values(cache)(yield)
			return
		}

		cursor := 0

		for {
			items := getMore(cursor)
			if len(items) == 0 { // no more items
				return
			}
			for _, item := range items {
				if !yield(item) {
					return
				}
			}
			cursor += len(items)
		}
	}
}

// Memoize2 returns a sequence of pairs that yields memoized pairs from the specified underlying sequence.
// Each pair of the specified sequence will only be forced at most once.
func Memoize2[Item1, Item2 any](seq Seq2[Item1, Item2]) Seq2[Item1, Item2] {
	var cache pairs[Item1, Item2]
	var complete atomic.Bool
	var lock sync.Mutex

	next, _ := Pull2(seq)

	getMore := func(offset int) pairs[Item1, Item2] {
		lock.Lock()
		defer lock.Unlock()

		for len(safeSuffix(cache, offset)) == 0 {
			item1, item2, ok := next()
			if !ok { // no more items in seq, cache complete
				complete.Store(true)
				break
			}
			cache = append(cache, internal.PairFrom(item1, item2))
		}

		return safeSuffix(cache, offset)
	}

	return func(yield func(Item1, Item2) bool) {
		if complete.Load() { // all items from seq are in the cache, no sync required
			cache.All(yield)
			return
		}

		cursor := 0

		for {
			pairs := getMore(cursor)
			if len(pairs) == 0 { // no more pairs
				return
			}
			for _, pair := range pairs {
				if !yield(pair.Unpack()) {
					return
				}
			}
			cursor += len(pairs)
		}
	}
}

// Min returns the smallest item in the specified sequence.
// It returns the zero value for Item when the sequence is empty.
func Min[Item cmp.Ordered](seq Seq[Item]) Item {
	return Reduce(seq, func(item1, item2 Item) Item { return min(item1, item2) })
}

// MuxMap2 turns a sequence of 2-way heterogeous items into a sequence of homogeneous items by using the specified multiplexing functions.
func MuxMap2[ItemIn1, ItemIn2, ItemOut any](seq MuxSeq2[ItemIn1, ItemIn2], mux1 func(ItemIn1) ItemOut, mux2 func(ItemIn2) ItemOut) Seq[ItemOut] {
	return func(yield func(ItemOut) bool) {
		seq(
			func(item ItemIn1) bool { return yield(mux1(item)) },
			func(item ItemIn2) bool { return yield(mux2(item)) },
		)
	}
}

// MuxMap3 turns a sequence of 3-way heterogeous items into a sequence of homogeneous items by using the specified multiplexing functions.
func MuxMap3[ItemIn1, ItemIn2, ItemIn3, ItemOut any](seq MuxSeq3[ItemIn1, ItemIn2, ItemIn3], mux1 func(ItemIn1) ItemOut, mux2 func(ItemIn2) ItemOut, mux3 func(ItemIn3) ItemOut) Seq[ItemOut] {
	return func(yield func(ItemOut) bool) {
		seq(
			func(item ItemIn1) bool { return yield(mux1(item)) },
			func(item ItemIn2) bool { return yield(mux2(item)) },
			func(item ItemIn3) bool { return yield(mux3(item)) },
		)
	}
}

// MuxMap4 turns a sequence of 4-way heterogeous items into a sequence of homogeneous items by using the specified multiplexing functions.
func MuxMap4[ItemIn1, ItemIn2, ItemIn3, ItemIn4, ItemOut any](seq MuxSeq4[ItemIn1, ItemIn2, ItemIn3, ItemIn4], mux1 func(ItemIn1) ItemOut, mux2 func(ItemIn2) ItemOut, mux3 func(ItemIn3) ItemOut, mux4 func(ItemIn4) ItemOut) Seq[ItemOut] {
	return func(yield func(ItemOut) bool) {
		seq(
			func(item ItemIn1) bool { return yield(mux1(item)) },
			func(item ItemIn2) bool { return yield(mux2(item)) },
			func(item ItemIn3) bool { return yield(mux3(item)) },
			func(item ItemIn4) bool { return yield(mux4(item)) },
		)
	}
}

// Or returns the logical OR of the boolean values in the specified sequence.
// The evaluation is short-circuiting.
func Or(seq Seq[bool]) bool {
	for v := range seq {
		if v {
			return true
		}
	}
	return false
}

// PackMap returns a sequence of items where each item is the result of packing a pair of values from the specified sequence using the specified function.
func PackMap[ItemIn1, ItemIn2, ItemOut any](seq Seq2[ItemIn1, ItemIn2], pack func(ItemIn1, ItemIn2) ItemOut) Seq[ItemOut] {
	return func(yield func(ItemOut) bool) {
		for item1, item2 := range seq {
			if !yield(pack(item1, item2)) {
				return
			}
		}
	}
}

// Panic returns a sequence of items that panics with the specified reason when enumerated.
func Panic[Item any](reason any) Seq[Item] {
	return func(_ func(Item) bool) {
		panic(reason)
	}
}

// Panic2 returns a sequence of pairs that panics with the specified reason when enumerated.
func Panic2[Item1, Item2 any](reason any) Seq2[Item1, Item2] {
	return func(_ func(Item1, Item2) bool) {
		panic(reason)
	}
}

// PullMany is like [Pull] for many sequences: it converts the specified “push-style” sequences into a “pull-style” iterator,
// pulling items from the sequences in lock-step.
// Next returns with false when any of the sequences is exhausted.
func PullMany[Item any](seqs ...Seq[Item]) (next func() ([]Item, bool), stop func()) {
	return int_iter.PullMany(wrapSeqSlice(seqs))
}

// PullMany2 is like [Pull2] for many sequences: it converts the specified “push-style” sequences into a “pull-style” iterator,
// pulling pairs from the sequences in lock-step.
// Next returns with false when any of the sequences is exhausted.
func PullMany2[Item1, Item2 any](seqs ...Seq2[Item1, Item2]) (next func() ([]Item1, []Item2, bool), stop func()) {
	if len(seqs) == 0 {
		return func() ([]Item1, []Item2, bool) { return nil, nil, false }, func() {}
	}

	nexts := make([]func() (Item1, Item2, bool), len(seqs))
	stops := make([]func(), len(seqs))
	for i := range seqs {
		nexts[i], stops[i] = Pull2(seqs[i])
	}
	return func() ([]Item1, []Item2, bool) {
			items1 := make([]Item1, len(nexts))
			items2 := make([]Item2, len(nexts))
			for i := range nexts {
				var ok bool
				items1[i], items2[i], ok = nexts[i]()
				if !ok {
					return nil, nil, false
				}
			}
			return items1, items2, true
		}, func() {
			for stop := range slices.Values(stops) {
				defer stop()
			}
		}
}

// Reduce returns the result of successively applying the specified combining function to items from the specified sequence.
// When the sequence is empty, the result will be the zero value for Item.
// When the sequence has a single item, that item will be the result.
func Reduce[Item any](seq Seq[Item], combine func(Item, Item) Item) (res Item) {
	res, _ = Last(Reductions(seq, combine))
	return
}

// Reduce2 returns the result of successively applying the specified combining function to pairs from the specified sequence.
// When the sequence is empty, the result will be the zero values for Item1 and Item2.
// When the sequence has a single pair, that pair will be the result.
func Reduce2[Item1, Item2 any](seq Seq2[Item1, Item2], combine func(Item1, Item2, Item1, Item2) (Item1, Item2)) (res1 Item1, res2 Item2) {
	res1, res2, _ = Last2(Reductions2(seq, combine))
	return
}

// Reductions returns a sequence of partial results of successively applying the specified combining function to items from the specified sequence.
// The first item of the returned sequence will be the first item of the specified sequence.
// When the specified sequence is empty, the returned sequence will be empty.
func Reductions[Item any](seq Seq[Item], combine func(Item, Item) Item) Seq[Item] {
	return func(yield func(Item) bool) {
		res := *new(Item)
		first := true
		for item := range seq {
			if first {
				first = false
				res = item
			} else {
				res = combine(res, item)
			}
			if !yield(res) {
				return
			}
		}
	}
}

// Reductions2 returns a sequence of partial results of successively applying the specified combining function to pairs from the specified sequence.
// The first pair of the returned sequence will be the first pair of the specified sequence.
// When the specified sequence is empty, the returned sequence will be empty.
func Reductions2[Item1, Item2 any](seq Seq2[Item1, Item2], combine func(Item1, Item2, Item1, Item2) (Item1, Item2)) Seq2[Item1, Item2] {
	type Pair = internal.Pair[Item1, Item2]
	return UnpackMap(Reductions(PackMap(seq, internal.PairFrom), func(r, i Pair) Pair {
		return internal.PairFrom(combine(r.Value1, r.Value2, i.Value1, i.Value2))
	}), Pair.Unpack)
}

// ReduceWhile returns the result of successively applying the specified combining function
// to items from the specified sequence while its second return value is true.
// When the sequence is empty, the result will be the zero value for Item.
// When the sequence has a single item, that item will be the result.
//
// TL;DR: it's [Reduce] with early return.
func ReduceWhile[Item any](seq Seq[Item], combine func(Item, Item) (Item, bool)) Item {
	res, _ := Last(ReductionsWhile(seq, combine))
	return res
}

// ReduceWhile2 returns the result of successively applying the specified combining function
// to pairs from the specified sequence while its third return value is true.
// When the sequence is empty, the result will be the zero value for Item1 and Item2.
// When the sequence has a single pair, that pair will be the result.
//
// TL;DR: it's [Reduce2] with early return.
func ReduceWhile2[Item1, Item2 any](seq Seq2[Item1, Item2], combine func(Item1, Item2, Item1, Item2) (Item1, Item2, bool)) (Item1, Item2) {
	res1, res2, _ := Last2(ReductionsWhile2(seq, combine))
	return res1, res2
}

// ReductionsWhile returns a sequence of partial results of successively applying the specified combining function
// to items from the specified sequence while its second return value is true.
// The first item of the returned sequence will be the first item of the specified sequence.
// When the specified sequence is empty, the returned sequence will be empty.
//
// TL;DR: it's [Reductions] with early return.
func ReductionsWhile[Item any](seq Seq[Item], combine func(Item, Item) (Item, bool)) Seq[Item] {
	return func(yield func(Item) bool) {
		res := *new(Item)
		first := true
		for item := range seq {
			var ok bool
			if first {
				first = false
				res, ok = item, true
			} else {
				res, ok = combine(res, item)
			}
			if !ok || !yield(res) {
				return
			}
		}
	}
}

// ReductionsWhile2 returns a sequence of partial results of successively applying the specified combining function
// to pairs from the specified sequence while its third return value is true.
// The first pair of the returned sequence will be the first pair of the specified sequence.
// When the specified sequence is empty, the returned sequence will be empty.
//
// TL;DR: it's [Reductions2] with early return.
func ReductionsWhile2[Item1, Item2 any](seq Seq2[Item1, Item2], combine func(Item1, Item2, Item1, Item2) (Item1, Item2, bool)) Seq2[Item1, Item2] {
	type Pair = internal.Pair[Item1, Item2]
	return UnpackMap(ReductionsWhile(PackMap(seq, internal.PairFrom), func(r, i Pair) (Pair, bool) {
		item1, item2, ok := combine(r.Value1, r.Value2, i.Value1, i.Value2)
		return internal.PairFrom(item1, item2), ok
	}), Pair.Unpack)
}

// Repeat returns a sequence infinitely repeating the specified value.
func Repeat[Item any](item Item) Seq[Item] {
	return func(yield func(Item) bool) {
		for yield(item) {
		}
	}
}

// Repeat2 returns a sequence infinitely repeating the specified pair of values.
func Repeat2[Item1, Item2 any](item1 Item1, item2 Item2) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		for yield(item1, item2) {
		}
	}
}

// RepeatN returns a sequence repeating the specified item [n] times.
func RepeatN[Item any](item Item, n int) Seq[Item] {
	return Take(Repeat(item), n)
}

// RepeatN2 returns a sequence repeating the specified pair of values [n] times.
func RepeatN2[Item1, Item2 any](item1 Item1, item2 Item2, n int) Seq2[Item1, Item2] {
	return Take2(Repeat2(item1, item2), n)
}

// Singleton returns a singleton sequence containing the specified item.
func Singleton[Item any](item Item) Seq[Item] {
	return func(yield func(Item) bool) {
		_ = yield(item)
	}
}

// Singleton2 returns a singleton sequence containing the specified pair.
func Singleton2[Item1, Item2 any](item1 Item1, item2 Item2) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		_ = yield(item1, item2)
	}
}

// Sum returns the sum of items in the specified sequence.
func Sum[Item Summable](seq Seq[Item]) Item {
	return Reduce(seq, add)
}

// Sums returns a sequence of partial sums of items in the specified sequence.
func Sums[Item Summable](seq Seq[Item]) Seq[Item] {
	return Reductions(seq, add)
}

// Swap returns a sequence of pairs of the swapped pairs of the specified sequence.
func Swap[Item1, Item2 any](seq Seq2[Item1, Item2]) Seq2[Item2, Item1] {
	return Map2(seq, swap)
}

// Take returns a sequence of at most n items from the start of the specified sequence.
func Take[Item any](seq Seq[Item], n int) Seq[Item] {
	if n <= 0 {
		return Empty[Item]
	}

	return func(yield func(Item) bool) {
		i := 0
		for item := range seq {
			i++
			if !yield(item) || i == n {
				return
			}
		}
	}
}

// Take2 returns a sequence of at most n pairs from the start of the specified sequence.
func Take2[Item1, Item2 any](seq Seq2[Item1, Item2], n int) Seq2[Item1, Item2] {
	if n <= 0 {
		return Empty2[Item1, Item2]
	}

	return func(yield func(Item1, Item2) bool) {
		i := 0
		for item1, item2 := range seq {
			i++
			if !yield(item1, item2) || i == n {
				return
			}
		}
	}
}

// TakeWhile returns a prefix of the specified sequence that contains only items that match the specified predicate.
func TakeWhile[Item any](seq Seq[Item], pred Pred[Item]) Seq[Item] {
	return func(yield func(Item) bool) {
		for item := range seq {
			if !pred(item) || !yield(item) {
				return
			}
		}
	}
}

// TakeWhile2 returns a prefix of the specified sequence that contains only pairs that match the specified predicate.
func TakeWhile2[Item1, Item2 any](seq Seq2[Item1, Item2], pred Pred2[Item1, Item2]) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		for item1, item2 := range seq {
			if !pred(item1, item2) || !yield(item1, item2) {
				return
			}
		}
	}
}

// Unfold returns a sequence of items generated by successively applying the specified function to the seed value.
//
// The specified function should return three values:
// * the next item when there is one OR the zero value of [Item] when there isn't
// * whether there is a next item
// * the seed value for the next invocation of the function.
func Unfold[Item, State any](seed State, next func(State) (Item, bool, State)) Seq[Item] {
	return func(yield func(Item) bool) {
		state := seed
		for {
			item, ok, nextState := next(state)
			if !ok || !yield(item) {
				return
			}
			state = nextState
		}
	}
}

// Unfold returns a sequence of pairs generated by successively applying the specified function to the seed value.
//
// The specified function should return four values:
// * the next first item of the pair when there is one OR the zero value of [Item1] when there isn't
// * the next second item of the pair when there is one OR the zero value of [Item2] when there isn't
// * whether there is a next pair of values
// * the seed value for the next invocation of the function.
func Unfold2[Item1, Item2, State any](seed State, next func(State) (Item1, Item2, bool, State)) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		state := seed
		for {
			item1, item2, ok, nextState := next(state)
			if !ok || !yield(item1, item2) {
				return
			}
			state = nextState
		}
	}
}

// UnpackMap returns a sequence of pairs by unpacking each of the items of the specified sequence to a pair of values.
func UnpackMap[ItemIn, ItemOut1, ItemOut2 any](seq Seq[ItemIn], unpack func(ItemIn) (ItemOut1, ItemOut2)) Seq2[ItemOut1, ItemOut2] {
	return func(yield func(ItemOut1, ItemOut2) bool) {
		for item := range seq {
			if !yield(unpack(item)) {
				return
			}
		}
	}
}

// Unzip returns two sequences that iterate over the first and second items of the specified sequence of pairs, respectively.
func Unzip[Item1, Item2 any](seq Seq2[Item1, Item2]) (Seq[Item1], Seq[Item2]) {
	return PackMap(seq, first), PackMap(seq, second)
}

// YieldAll yields all items from the specified sequence using the specified function.
// It returns false if yield returned false.
//
// This can be useful when forwarding enumeration to a child sequence or emulating Python's for-else.
func YieldAll[Item any](seq Seq[Item], yield func(Item) bool) bool {
	for item := range seq {
		if !yield(item) {
			return false
		}
	}
	return true
}

// YieldAll2 yields all pairs from the specified sequence using the specified function.
// It returns false if yield returned false.
//
// This can be useful when forwarding enumeration to a child sequence or emulating Python's for-else.
func YieldAll2[Item1, Item2 any](seq Seq2[Item1, Item2], yield func(Item1, Item2) bool) bool {
	for item1, item2 := range seq {
		if !yield(item1, item2) {
			return false
		}
	}
	return true
}

// Zip returns a sequence of pairs obtained by taking corresponding items from the specified sequences.
func Zip[Item1, Item2 any](seq1 Seq[Item1], seq2 Seq[Item2]) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		next1, stop1 := Pull(seq1)
		defer stop1()
		next2, stop2 := Pull(seq2)
		defer stop2()

		for {
			item1, ok1 := next1()
			item2, ok2 := next2()
			if !ok1 || !ok2 || !yield(item1, item2) {
				return
			}
		}
	}
}

// ZipMany returns a sequence of slices obtained by taking corresponding items from the specified sequences.
func ZipMany[Item any](seqs ...Seq[Item]) Seq[[]Item] {
	return int_iter.ZipMany(wrapSeqSlice(seqs))
}

func add[Value Summable](a, b Value) Value {
	return a + b
}

func swap[Value1, Value2 any](v1 Value1, v2 Value2) (Value2, Value1) {
	return v2, v1
}

func safeSuffix[Slice ~[]Item, Item any](slice Slice, from int) Slice {
	from = max(min(from, len(slice)), 0)
	return slice[from:]
}

func wrapSeqSlice[Item any](sliceOfSeq []Seq[Item]) seqSlice[Item] {
	return seqSlice[Item](sliceOfSeq)
}

type seqSlice[Item any] []Seq[Item]

func (seqs seqSlice[Item]) ItemsWithIndex(yield func(int, Seq[Item]) bool) {
	slices.All(seqs)(yield)
}

func (seqs seqSlice[Item]) Len() int {
	return len(seqs)
}

func first[Value1, Value2 any](value1 Value1, _ Value2) Value1 {
	return value1
}

func second[Value1, Value2 any](_ Value1, value2 Value2) Value2 {
	return value2
}
