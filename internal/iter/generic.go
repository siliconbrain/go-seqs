package iter

import (
	"iter"
	"slices"
)

type SliceOfSeq[Item any] interface {
	ItemsWithIndex(yield func(int, iter.Seq[Item]) bool)
	Len() int
}

func Cycle[Item any](seq iter.Seq[Item]) iter.Seq[Item] {
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

func Interleave[Item any](seqs SliceOfSeq[Item]) iter.Seq[Item] {
	return func(yield func(Item) bool) {
		nexts := make([]func() (Item, bool), seqs.Len())
		for i, seq := range seqs.ItemsWithIndex {
			var stop func()
			nexts[i], stop = iter.Pull(seq)
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

func PullMany[Item any](seqs SliceOfSeq[Item]) (next func() ([]Item, bool), stop func()) {
	if seqs.Len() == 0 {
		return func() ([]Item, bool) { return nil, false }, func() {}
	}

	nexts := make([]func() (Item, bool), seqs.Len())
	stops := make([]func(), seqs.Len())
	for i, seq := range seqs.ItemsWithIndex {
		nexts[i], stops[i] = iter.Pull(seq)
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

func ZipMany[Item any](seqs SliceOfSeq[Item]) iter.Seq[[]Item] {
	return func(yield func([]Item) bool) {
		next, stop := PullMany(seqs)
		defer stop()
		for {
			if items, ok := next(); !ok || !yield(items) {
				return
			}
		}
	}
}
