package iter

import (
	"slices"
	"sync"
	"sync/atomic"
)

// Summable lists types that support addition using the + operator
type Summable interface {
	~float32 | ~float64 | ~int | ~int8 | ~int16 | ~int32 | ~int64 | ~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | string
}

// All returns whether the specified predicate matches all items in the specified sequence.
func All[Item any](seq Seq[Item], pred func(Item) bool) bool {
	return And(Map(seq, pred))
}

func All2[Item1, Item2 any](seq Seq2[Item1, Item2], pred func(Item1, Item2) bool) bool {
	return And(PackMap(seq, pred))
}

// And returns the logical AND of the boolean values in [seq].
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
func Any[Item any](seq Seq[Item], pred func(Item) bool) bool {
	return Or(Map(seq, pred))
}

func Any2[Item1, Item2 any](seq Seq2[Item1, Item2], pred func(Item1, Item2) bool) bool {
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

// Cartesian returns a sequence of pairs where each pair is a member of the cartesian product of (i.e. all combinations of items from) the two supplied sequences.
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

// Cycle returns a sequence of items that infinitely repeates items from the specified sequence.
func Cycle[Item any](seq Seq[Item]) Seq[Item] {
	return func(yield func(Item) bool) {
		for {
			empty := true
			for item := range seq {
				empty = false
				if !yield(item) {
					return
				}
			}
			if empty {
				return
			}
		}
	}
}

// Cycle2 returns a sequence of pairs that infinitely repeates pairs from the specified sequence.
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

// Divvy returns a sequence of slices with at most size length containing a continuous range of elements from the specified sequence.
// The start of each slice will be offset by skip number of elements from the previous one.
// Slices will overlap when size > skip, and some elements will be dropped when size < skip.
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

// DivvyExact is like [Divvy] but all slices are exactly size length.
// Any trailing elements are dropped.
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

// Empty is an empty sequence of items.
func Empty[Item any](yield func(Item) bool) {}

// Empty2 is an empty sequence of pairs.
func Empty2[Item1, Item2 any](yield func(Item1, Item2) bool) {}

// Enumerate returns a sequence of pairs where each pair consists of the ordinal of an item from the specified sequence and the item itself.
func Enumerate[Item any](seq Seq[Item]) Seq2[int, Item] {
	return Zip(Count(0, 1), seq)
}

// Filter returns a sequence of items that only contains items of the specified sequence that match the specified predicate.
func Filter[Item any](seq Seq[Item], pred func(Item) bool) Seq[Item] {
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
func Filter2[Item1, Item2 any](seq Seq2[Item1, Item2], pred func(Item1, Item2) bool) Seq2[Item1, Item2] {
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
	return PackMap(Filter2(UnpackMap(seq, mapFn), snd), fst)
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

// Flatten returns the concatenation of sequences inside the specified sequence.
func Flatten[Item any](seqs Seq[Seq[Item]]) Seq[Item] {
	return func(yield func(Item) bool) {
		for seq := range seqs {
			for item := range seq {
				if !yield(item) {
					return
				}
			}
		}
	}
}

// Flatten2 returns the concatenation of sequences inside the specified sequence.
func Flatten2[Item1, Item2 any](seqs Seq[Seq2[Item1, Item2]]) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		for seq := range seqs {
			for item1, item2 := range seq {
				if !yield(item1, item2) {
					return
				}
			}
		}
	}
}

// Fold returns the result of successively applying the specified combining function to items from the specified sequence, starting with the seed value.
// When the sequence is empty, the result will be the seed value.
func Fold[Item, Result any](seq Seq[Item], seed Result, combineFn func(Result, Item) Result) (res Result) {
	res, _ = Last(Folds(seq, seed, combineFn))
	return
}

// Fold2 returns the result of successively applying the specified combining function to pairs from the specified sequence, starting with the seed value.
// When the sequence is empty, the result will be the seed value.
func Fold2[Item1, Item2, Result any](seq Seq2[Item1, Item2], seed Result, combineFn func(Result, Item1, Item2) Result) (res Result) {
	res = seed
	for item1, item2 := range seq {
		res = combineFn(res, item1, item2)
	}
	return
}

// Folds returns a sequence of partial results of successively applying the specified combining function to items from the specified sequence, starting with the seed value.
func Folds[Item, Result any](seq Seq[Item], seed Result, combineFn func(Result, Item) Result) Seq[Result] {
	return func(yield func(Result) bool) {
		res := seed
		if !yield(res) {
			return
		}
		for item := range seq {
			res = combineFn(res, item)
			if !yield(res) {
				return
			}
		}
	}
}

// Folds2 returns a sequence of partial results of successively applying the specified combining function to pairs from the specified sequence, starting with the seed value.
func Folds2[Item1, Item2, Result any](seq Seq2[Item1, Item2], seed Result, combineFn func(Result, Item1, Item2) Result) Seq[Result] {
	return func(yield func(Result) bool) {
		res := seed
		if !yield(res) {
			return
		}
		for item1, item2 := range seq {
			res = combineFn(res, item1, item2)
			if !yield(res) {
				return
			}
		}
	}
}

// FromValues returns a sequence that yields the specified values.
func FromValues[Item any](values ...Item) Seq[Item] {
	return slices.Values(values)
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

// Len returns the length of the sequence by counting its items.
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
			cache = append(cache, pair[Item1, Item2]{item1, item2})
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

// Or returns the logical OR of the boolean values in [seq].
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

// PullMany is like [Pull] for many sequences: it converts the specified “push-style” sequences into a “pull-style” iterator,
// pulling items from the sequences in lock-step.
// Next returns with false when any of the sequences is exhausted.
func PullMany[Item any](seqs ...Seq[Item]) (next func() ([]Item, bool), stop func()) {
	if len(seqs) == 0 {
		return func() ([]Item, bool) { return nil, false }, func() {}
	}

	nexts := make([]func() (Item, bool), len(seqs))
	stops := make([]func(), len(seqs))
	for i := range seqs {
		nexts[i], stops[i] = Pull(seqs[i])
	}
	return func() ([]Item, bool) {
			items := make([]Item, len(nexts))
			for i := range nexts {
				var ok bool
				items[i], ok = nexts[i]()
				if !ok {
					return nil, false
				}
			}
			return items, true
		}, func() {
			for stop := range slices.Values(stops) {
				defer stop()
			}
		}
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
// When the sequence is empty, the result will be the zero value for [Item].
// When the sequence has a single item, that item will be the result.
func Reduce[Item any](seq Seq[Item], combineFn func(Item, Item) Item) (res Item) {
	res, _ = Last(Reductions(seq, combineFn))
	return
}

// Reduce2 returns the result of successively applying the specified combining function to pairs from the specified sequence.
// When the sequence is empty, the result will be the zero values for [Item1] and [Item2].
// When the sequence has a single pair, that pair will be the result.
func Reduce2[Item1, Item2 any](seq Seq2[Item1, Item2], combineFn func(Item1, Item2, Item1, Item2) (Item1, Item2)) (res1 Item1, res2 Item2) {
	res1, res2, _ = Last2(Reductions2(seq, combineFn))
	return
}

// Reductions returns a sequence of partial results of successively applying the specified combining function to items from the specified sequence.
// The first item of the returned sequence will be the first item of the specified sequence.
// When the specified sequence is empty, the returned sequence will be empty.
func Reductions[Item any](seq Seq[Item], combineFn func(Item, Item) Item) Seq[Item] {
	return func(yield func(Item) bool) {
		res := *new(Item)
		first := true
		for item := range seq {
			if first {
				first = false
				res = item
			} else {
				res = combineFn(res, item)
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
func Reductions2[Item1, Item2 any](seq Seq2[Item1, Item2], combineFn func(Item1, Item2, Item1, Item2) (Item1, Item2)) Seq2[Item1, Item2] {
	return func(yield func(Item1, Item2) bool) {
		res1, res2 := *new(Item1), *new(Item2)
		first := true
		for item1, item2 := range seq {
			if first {
				first = false
				res1, res2 = item1, item2
			} else {
				res1, res2 = combineFn(res1, res2, item1, item2)
			}
			if !yield(res1, res2) {
				return
			}
		}
	}
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

// Skip returns a sequence with at most n items skipped from the start of the specified sequence.
func Skip[Item any](seq Seq[Item], n int) Seq[Item] {
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

// Skip2 returns a sequence with at most n pairs skipped from the start of the specified sequence.
func Skip2[Item1, Item2 any](seq Seq2[Item1, Item2], n int) Seq2[Item1, Item2] {
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

// SkipWhile returns the rest of the specified sequence after the prefix of items matching the specified predicate.
func SkipWhile[Item any](seq Seq[Item], pred func(Item) bool) Seq[Item] {
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

// SkipWhile2 returns the rest of the specified sequence after the prefix of pairs matching the specified predicate.
func SkipWhile2[Item1, Item2 any](seq Seq2[Item1, Item2], pred func(Item1, Item2) bool) Seq2[Item1, Item2] {
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
func TakeWhile[Item any](seq Seq[Item], pred func(Item) bool) Seq[Item] {
	return func(yield func(Item) bool) {
		for item := range seq {
			if !pred(item) || !yield(item) {
				return
			}
		}
	}
}

// TakeWhile2 returns a prefix of the specified sequence that contains only pairs that match the specified predicate.
func TakeWhile2[Item1, Item2 any](seq Seq2[Item1, Item2], pred func(Item1, Item2) bool) Seq2[Item1, Item2] {
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
	return PackMap(seq, fst), PackMap(seq, snd)
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
	return func(yield func([]Item) bool) {
		next, stop := PullMany(seqs...)
		defer stop()
		for {
			if items, ok := next(); !ok || !yield(items) {
				return
			}
		}
	}
}

func add[Value Summable](a, b Value) Value {
	return a + b
}

func fst[Value1, Value2 any](v Value1, _ Value2) Value1 {
	return v
}

func snd[Value1, Value2 any](_ Value1, v Value2) Value2 {
	return v
}

func swap[Value1, Value2 any](v1 Value1, v2 Value2) (Value2, Value1) {
	return v2, v1
}

func safeSuffix[Slice ~[]Item, Item any](slice Slice, from int) Slice {
	from = max(min(from, len(slice)), 0)
	return slice[from:]
}

type pairs[Fst, Snd any] []pair[Fst, Snd]

func (ps pairs[Fst, Snd]) All(yield func(Fst, Snd) bool) {
	for _, p := range ps {
		if !yield(p.Fst, p.Snd) {
			return
		}
	}
}

type pair[Fst, Snd any] struct {
	Fst Fst
	Snd Snd
}

func (p pair[Fst, Snd]) Unpack() (Fst, Snd) {
	return p.Fst, p.Snd
}
