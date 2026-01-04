package seqs

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestAppendTo(t *testing.T) {
	t.Run("empty seq should not change the slice", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3}, AppendToSlice(Empty[int](), []int{1, 2, 3}))
	})
	t.Run("seq items should be appended to the slice in yield order", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3, 4, 5}, AppendToSlice(FromValues(3, 4, 5), []int{1, 2}))
	})
	t.Run("seq can be appended to nil slice", func(t *testing.T) {
		assert.Equal(t, []int{1, 2, 3}, AppendToSlice(FromValues(1, 2, 3), []int(nil)))
	})
	t.Run("should panic when seq is known to be infinite", func(t *testing.T) {
		assert.Panics(t, func() { _ = AppendToSlice(Repeat(1), []int{1}) })
	})
}

func TestCartesian(t *testing.T) {
	finite1 := FromValues(1, 2, 3)
	unknown1 := hideCard(finite1)
	infinite1 := Repeat(1)
	finite2 := FromValues("a", "b", "c")
	unknown2 := hideCard(finite2)
	infinite2 := Repeat("a")

	testCases := map[string]struct {
		seq1           Seq[int]
		seq2           Seq[string]
		wantIsFinite   bool
		wantIsInfinite bool
	}{
		"finite x finite": {
			seq1:           finite1,
			seq2:           finite2,
			wantIsFinite:   true,
			wantIsInfinite: false,
		},
		"finite x unknown": {
			seq1:           finite1,
			seq2:           unknown2,
			wantIsFinite:   false,
			wantIsInfinite: false,
		},
		"finite x infinite": {
			seq1:           finite1,
			seq2:           infinite2,
			wantIsFinite:   false,
			wantIsInfinite: true,
		},
		"unknown x finite": {
			seq1:           unknown1,
			seq2:           finite2,
			wantIsFinite:   false,
			wantIsInfinite: false,
		},
		"unknown x unknown": {
			seq1:           unknown1,
			seq2:           unknown2,
			wantIsFinite:   false,
			wantIsInfinite: false,
		},
		"unknown x infinite": {
			seq1:           unknown1,
			seq2:           infinite2,
			wantIsFinite:   false,
			wantIsInfinite: true,
		},
		"infinite x finite": {
			seq1:           infinite1,
			seq2:           finite2,
			wantIsFinite:   false,
			wantIsInfinite: true,
		},
		"infinite x unknown": {
			seq1:           infinite1,
			seq2:           unknown2,
			wantIsFinite:   false,
			wantIsInfinite: true,
		},
		"infinite x infinite": {
			seq1:           infinite1,
			seq2:           infinite2,
			wantIsFinite:   false,
			wantIsInfinite: true,
		},
	}
	for name, testCase := range testCases {
		t.Run(name, func(t *testing.T) {
			seq := Cartesian(testCase.seq1, testCase.seq2, pairFrom)
			assert.Equal(t, testCase.wantIsFinite, IsFinite(seq))
			assert.Equal(t, testCase.wantIsInfinite, IsInfinite(seq))
		})
	}
}

func TestConcat(t *testing.T) {
	t.Run("should return empty seq when called without params", func(t *testing.T) {
		assert.True(t, IsEmpty(Concat[int]()))
	})
	t.Run("should return single parameter as-is", func(t *testing.T) {
		seq := FromValues(1, 2, 3)
		assert.Equal(t, seq, Concat(seq))
	})
	t.Run("should return finite seq when all params are finite", func(t *testing.T) {
		assert.True(t, IsFinite(Concat(FromValues(1, 2), FromValues(3, 4), FromValues(5, 6))))
	})
	t.Run("should not return finite seq when any param's cardinality is unknown", func(t *testing.T) {
		assert.False(t, IsFinite(Concat(FromValues(1, 2), hideCard(FromValues(3, 4)), FromValues(5, 6))))
	})
	t.Run("should return infinite seq when any param is infinite", func(t *testing.T) {
		assert.True(t, IsInfinite(Concat(FromValues(1, 2), hideCard(FromValues(3, 4)), Repeat(5))))
	})
}

func TestCount(t *testing.T) {
	t.Run("should return infinite seq", func(t *testing.T) {
		assert.True(t, IsInfinite(Count(1, 1)))
	})
}

func TestCycle(t *testing.T) {
	t.Run("should return an infinite sequence unchanged", func(t *testing.T) {
		seq := comparableInfiniteSeq[int]{item: 42}
		assert.Equal(t, seq, Cycle(seq))
	})
	t.Run("should return empty sequence for empty sequence", func(t *testing.T) {
		assert.True(t, IsEmpty(Cycle(Empty[int]())))
	})
}

func TestDivvy(t *testing.T) {
	t.Run("unlenable seq", func(t *testing.T) {
		_, hasLen := getLength(Divvy(hideCard(FromValues(1, 2, 3)), 1, 1))
		require.False(t, hasLen, "should not know length for unknown length sequence")
	})

	check := func(seq FiniteSeq[int], size int, skip int) func(t *testing.T) {
		return func(t *testing.T) {
			windows := Divvy(seq, size, skip)
			expected := Len(hideCard(windows))
			actual, _ := getLength(windows)
			t.Logf("Divvy(%v, %d, %d) => %v", ToSlice(seq), size, skip, ToSlice(windows))
			require.Equal(t, expected, actual)
		}
	}
	t.Run("empty seq", check(Empty[int]().(FiniteSeq[int]), 1, 1))
	t.Run("each element by itself", check(FromValues(1, 2, 3, 4).(FiniteSeq[int]), 1, 1))
	t.Run("adjacent pairs", check(FromValues(1, 2, 3, 4).(FiniteSeq[int]), 2, 2))
	t.Run("adjacent pairs with remainder", check(FromValues(1, 2, 3, 4, 5).(FiniteSeq[int]), 2, 2))
	t.Run("overlapping pairs", check(FromValues(1, 2, 3, 4, 5).(FiniteSeq[int]), 2, 1))
	t.Run("overlapping triplets", check(FromValues(1, 2, 3, 4, 5).(FiniteSeq[int]), 3, 2))
	t.Run("overlapping triplets with remainder", check(FromValues(1, 2, 3, 4, 5, 6).(FiniteSeq[int]), 3, 2))

	t.Run("should return infinite sequence for infinite sequence", func(t *testing.T) {
		assert.True(t, IsInfinite(Divvy(Repeat(1), 1, 1)))
	})
}

func TestDivvyExact(t *testing.T) {
	t.Run("unlenable seq", func(t *testing.T) {
		_, hasLen := getLength(DivvyExact(hideCard(FromValues(1, 2, 3)), 1, 1))
		require.False(t, hasLen, "should not know length for unknown length sequence")
	})

	check := func(seq FiniteSeq[int], size int, skip int) func(t *testing.T) {
		return func(t *testing.T) {
			windows := DivvyExact(seq, size, skip)
			expected := Len(hideCard(windows))
			actual, _ := getLength(windows)
			t.Logf("DivvyExact(%v, %d, %d) => %v", ToSlice(seq), size, skip, ToSlice(windows))
			require.Equal(t, expected, actual)
		}
	}
	t.Run("empty seq", check(Empty[int]().(FiniteSeq[int]), 1, 1))
	t.Run("each element by itself", check(FromValues(1, 2, 3, 4).(FiniteSeq[int]), 1, 1))
	t.Run("adjacent pairs", check(FromValues(1, 2, 3, 4).(FiniteSeq[int]), 2, 2))
	t.Run("adjacent pairs with remainder", check(FromValues(1, 2, 3, 4, 5).(FiniteSeq[int]), 2, 2))
	t.Run("overlapping pairs", check(FromValues(1, 2, 3, 4, 5).(FiniteSeq[int]), 2, 1))
	t.Run("overlapping triplets", check(FromValues(1, 2, 3, 4, 5).(FiniteSeq[int]), 3, 2))
	t.Run("overlapping triplets with remainder", check(FromValues(1, 2, 3, 4, 5, 6).(FiniteSeq[int]), 3, 2))

	t.Run("should return empty sequence for empty sequence", func(t *testing.T) {
		assert.True(t, IsInfinite(DivvyExact(Repeat(1), 1, 1)))
	})
}

func TestDropLast(t *testing.T) {
	t.Run("should return infinite seq as-is", func(t *testing.T) {
		seq := comparableInfiniteSeq[int]{item: 42}
		assert.Equal(t, seq, DropLast(seq, 42))
	})
	t.Run("should return empty seq for empty seq", func(t *testing.T) {
		assert.True(t, IsEmpty(DropLast(Empty[int](), 42)))
	})
	t.Run("should return seq as-is when dropping no items", func(t *testing.T) {
		seq := FromValues(1, 2, 3)
		assert.Equal(t, seq, DropLast(seq, 0))
	})
	t.Run("should return finite seq when dropping items from finite seq", func(t *testing.T) {
		finiteSeq, isFiniteSeq := AsFiniteSeq(DropLast(FromValues(1, 2, 3, 4), 2))
		assert.True(t, isFiniteSeq)
		assert.Equal(t, 2, finiteSeq.Len())
	})
	t.Run("should return zero as length when dropping more items than present in a finite seq", func(t *testing.T) {
		finiteSeq, _ := AsFiniteSeq(DropLast(FromValues(1, 2, 3), 5))
		assert.Equal(t, 0, finiteSeq.Len())
	})
}

func TestEmpty(t *testing.T) {
	assert.Empty(t, ToSlice(Empty[int]()))
	assert.Equal(t, 0, Len(Empty[int]()))
	assert.True(t, IsFinite(Empty[int]()))
}

func TestEnumerate(t *testing.T) {
	t.Run("should return finite seq for finite seq", func(t *testing.T) {
		assert.True(t, IsFinite(Enumerate(FromValues(1, 2, 3))))
	})
	t.Run("should return infinite seq for infinite seq", func(t *testing.T) {
		assert.True(t, IsInfinite(Enumerate(Repeat(42))))
	})
}

func TestFilter(t *testing.T) {
	t.Run("should return infinite, possibly divergent seq for infinite seq", func(t *testing.T) {
		seq := Filter(Repeat(42), func(int) bool { return false })
		assert.True(t, IsInfinite(seq), "should be marked infinite")
		assert.True(t, CanDiverge(seq), "should be marked divergent")
	})
}

func TestFilterMap(t *testing.T) {
	t.Run("should return infinite, possibly divergent seq for infinite seq", func(t *testing.T) {
		seq := FilterMap(Repeat(42), func(int) (int, bool) { return 0, false })
		assert.True(t, IsInfinite(seq), "should be marked infinite")
		assert.True(t, CanDiverge(seq), "should be marked divergent")
	})
}

func TestFilterWithIndex(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		pred func(int, int) bool
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			pred: func(idx int, itm int) bool { return true },
			want: Empty[int](),
		},
		"every other": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(idx int, itm int) bool { return idx%2 == 0 },
			want: FromValues(1, 3),
		},
		"take all": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(idx int, itm int) bool { return true },
			want: FromValues(1, 2, 3, 4),
		},
		"take none": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(idx int, itm int) bool { return false },
			want: Empty[int](),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(FilterWithIndex(testCase.seq, testCase.pred)))
		})
	}
}

func TestFirst(t *testing.T) {
	testCases := map[string]struct {
		seq          Seq[int]
		wantFirst    int
		wantHasFirst bool
	}{
		"empty seq": {
			seq:          Empty[int](),
			wantFirst:    0,
			wantHasFirst: false,
		},
		"singleton seq": {
			seq:          FromValues(42),
			wantFirst:    42,
			wantHasFirst: true,
		},
		"multi-element seq": {
			seq:          FromValues(1, 2, 3, 4),
			wantFirst:    1,
			wantHasFirst: true,
		},
		"infinite seq": {
			seq:          Count(2, 1),
			wantFirst:    2,
			wantHasFirst: true,
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			first, hasFirst := First(testCase.seq)
			assert.Equal(t, testCase.wantFirst, first)
			assert.Equal(t, testCase.wantHasFirst, hasFirst)
		})
	}
}

func TestFlatten(t *testing.T) {
	testCases := map[string]struct {
		seqs Seq[Seq[int]]
		want Seq[int]
	}{
		"no seqs": {
			seqs: Empty[Seq[int]](),
			want: Empty[int](),
		},
		"one seq": {
			seqs: FromValues(
				FromValues(1, 2, 3, 4),
			),
			want: FromValues(1, 2, 3, 4),
		},
		"two seqs": {
			seqs: FromValues(
				FromValues(1, 2),
				FromValues(3, 4),
			),
			want: FromValues(1, 2, 3, 4),
		},
		"more seqs": {
			seqs: FromValues(
				FromValues(1),
				FromValues(2),
				FromValues(3),
				FromValues(4),
			),
			want: FromValues(1, 2, 3, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Flatten(testCase.seqs)))
		})
	}
}

func TestFold(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		require.Equal(t, 42, Fold(Empty[int](), 42, func(a int, e int) int { return a + e }))
	})
	t.Run("offset sum", func(t *testing.T) {
		require.Equal(t, 20, Fold(FromValues(1, 2, 3, 4), 10, func(a int, e int) int { return a + e }))
	})
}

func TestFolds(t *testing.T) {
	const maxSamples = 100
	testCases := map[string]struct {
		seq  Seq[int]
		seed int
		op   func(int, int) int
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			seed: 42,
			op:   add[int],
			want: FromValues(42),
		},
		"singleton seq": {
			seq:  FromValues(42),
			seed: 21,
			op:   add[int],
			want: FromValues(21, 63),
		},
		"multi-element seq": {
			seq:  FromValues(1, 2, 3, 4),
			seed: 1,
			op:   add[int],
			want: FromValues(1, 2, 4, 7, 11),
		},
		"infinite seq": {
			seq:  Repeat(1),
			seed: 0,
			op:   add[int],
			want: Count(0, 1),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, ToSlice(Take(testCase.want, maxSamples)), ToSlice(Take(Folds(testCase.seq, testCase.seed, testCase.op), maxSamples)))
		})
	}

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, []int{42}, ToSlice(Take(Folds(Repeat(1), 42, add), 1)))
	})
}

func TestFromValue(t *testing.T) {
	seq := Singleton(42)
	require.Equal(t, []int{42}, ToSlice(seq))
	require.Equal(t, 1, seq.(FiniteSeq[int]).Len())
}

func TestFromSlicePtrs(t *testing.T) {
	vals := []int{1, 2, 3, 4}
	seq := FromSlicePtrs(vals)
	for p := range ToIter(seq) {
		*p = *p * 2
	}
	assert.Equal(t, []int{2, 4, 6, 8}, vals)

	// cover Len() method
	length, hasLength := getLength(seq)
	require.True(t, hasLength)
	assert.Equal(t, len(vals), length)

	// cover early exit
	assert.Equal(t, vals[:3], ToSlice(Take(Map(seq, func(p *int) int { return *p }), 3)))
}

func TestGenerate(t *testing.T) {
	testCases := map[string]struct {
		fn   func() int
		want []int
	}{
		"return 4 randomly": {
			fn: func() int {
				return 4
			},
			want: []int{4, 4, 4, 4},
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, testCase.want, ToSlice(Take(Generate(testCase.fn), len(testCase.want))))
		})
	}
}

func TestIsEmpty(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		want bool
	}{
		"empty seq": {
			seq:  Empty[int](),
			want: true,
		},
		"some seq": {
			seq:  FromValues(1, 2, 3),
			want: false,
		},
		"infinite seq": {
			seq:  Repeat(42),
			want: false,
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, testCase.want, IsEmpty(testCase.seq))
		})
	}
}

func TestJoin(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		val  int
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			val:  42,
			want: Empty[int](),
		},
		"one element seq": {
			seq:  FromValues(1),
			val:  42,
			want: FromValues(1),
		},
		"two element seq": {
			seq:  FromValues(1, 2),
			val:  42,
			want: FromValues(1, 42, 2),
		},
		"many element seq": {
			seq:  FromValues(1, 2, 3, 4),
			val:  42,
			want: FromValues(1, 42, 2, 42, 3, 42, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Intersperse(testCase.seq, testCase.val)))
		})
	}
	t.Run("break early", func(t *testing.T) {
		t.Run("on odd element", func(t *testing.T) {
			require.Equal(t, ToSlice(FromValues(1, 42, 1)), ToSlice(Take(Intersperse(RepeatN(1, 4), 42), 3)))
		})
		t.Run("on even element", func(t *testing.T) {
			require.Equal(t, ToSlice(FromValues(1, 42, 1, 42)), ToSlice(Take(Intersperse(RepeatN(1, 4), 42), 4)))
		})
	})
	t.Run("infinite seq", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(1, 42, 1, 42, 1, 42)), ToSlice(Take(Intersperse(Repeat(1), 42), 6)))
	})
}

func TestLast(t *testing.T) {
	testCases := map[string]struct {
		seq         Seq[int]
		wantLast    int
		wantHasLast bool
	}{
		"empty seq": {
			seq:         Empty[int](),
			wantLast:    0,
			wantHasLast: false,
		},
		"singleton seq": {
			seq:         FromValues(42),
			wantLast:    42,
			wantHasLast: true,
		},
		"multi-element seq": {
			seq:         FromValues(1, 2, 3, 4),
			wantLast:    4,
			wantHasLast: true,
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			last, hasLast := Last(testCase.seq)
			assert.Equal(t, testCase.wantLast, last)
			assert.Equal(t, testCase.wantHasLast, hasLast)
		})
	}
}

func TestLen(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		want int
	}{
		"empty seq": {
			seq:  Empty[int](),
			want: 0,
		},
		"non-empty seq": {
			seq:  FromValues(1, 2, 3),
			want: 3,
		},
		"unknown length seq": {
			seq:  Filter(FromValues(0, 1, 2, 3, 4, 5), func(v int) bool { return v%2 == 0 }),
			want: 3,
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, testCase.want, Len(testCase.seq))
		})
	}
}

func TestMap(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		require.Equal(t, ToSlice(Empty[int]()), ToSlice(Map(Empty[int](), func(i int) int { return i })))
	})
	t.Run("times two", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(2, 4, 6, 8)), ToSlice(Map(FromValues(1, 2, 3, 4), func(i int) int { return i * 2 })))
	})
	t.Run("to string", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues("1", "2", "3", "4")), ToSlice(Map(FromValues(1, 2, 3, 4), func(i int) string { return fmt.Sprint(i) })))
	})
	t.Run("infinite seq", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(2, 2, 2, 2)), ToSlice(Take(Map(Repeat(1), func(i int) int { return i * 2 }), 4)))
	})
}

func TestMapWithIndex(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		require.Equal(t, ToSlice(Empty[int]()), ToSlice(MapWithIndex(Empty[int](), func(idx int, itm int) int { return idx + itm })))
	})
	t.Run("add index", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(1, 3, 5, 7)), ToSlice(MapWithIndex(FromValues(1, 2, 3, 4), func(idx int, itm int) int { return itm + idx })))
	})
	t.Run("to string with index", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues("0:1", "1:2", "2:3", "3:4")), ToSlice(MapWithIndex(FromValues(1, 2, 3, 4), func(idx int, itm int) string { return fmt.Sprint(idx, ":", itm) })))
	})
	t.Run("infinite seq", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(1, 2, 3, 4)), ToSlice(Take(MapWithIndex(Repeat(1), func(idx int, itm int) int { return idx + itm }), 4)))
	})
}

func TestReductions(t *testing.T) {
	const maxSamples = 100
	testCases := map[string]struct {
		seq  Seq[int]
		op   func(int, int) int
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			op:   add[int],
			want: Empty[int](),
		},
		"singleton seq": {
			seq:  FromValues(42),
			op:   add[int],
			want: FromValues(42),
		},
		"multi-element seq": {
			seq:  FromValues(1, 2, 3, 4),
			op:   add[int],
			want: FromValues(1, 3, 6, 10),
		},
		"infinite seq": {
			seq:  Repeat(1),
			op:   add[int],
			want: Count(1, 1),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, ToSlice(Take(testCase.want, maxSamples)), ToSlice(Take(Reductions(testCase.seq, testCase.op), maxSamples)))
		})
	}
}

func TestRepeat(t *testing.T) {
	require.Equal(t, ToSlice(RepeatN(42, 6)), ToSlice(Take(Repeat(42), 6)))
}

func TestSkip(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		n    int
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			n:    42,
			want: Empty[int](),
		},
		"skip 3": {
			seq:  FromValues(1, 2, 3, 4),
			n:    3,
			want: FromValues(4),
		},
		"skip more": {
			seq:  FromValues(1, 2, 3, 4),
			n:    42,
			want: Empty[int](),
		},
		"skip none": {
			seq:  FromValues(1, 2, 3, 4),
			n:    0,
			want: FromValues(1, 2, 3, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Drop(testCase.seq, testCase.n)))
		})
	}
	t.Run("infinite seq", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(3, 4, 1, 2, 3, 4, 1, 2)), ToSlice(Take(Drop(Cycle(FromValues(1, 2, 3, 4)), 2), 8)))
	})
}

func TestSkipWhile(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		pred func(int) bool
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			pred: func(i int) bool { return false },
			want: Empty[int](),
		},
		"less than 3": {
			seq:  FromValues(1, 2, 3, 4, 3, 2, 1),
			pred: func(i int) bool { return i < 3 },
			want: FromValues(3, 4, 3, 2, 1),
		},
		"skip all": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return true },
			want: Empty[int](),
		},
		"skip none": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return false },
			want: FromValues(1, 2, 3, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(DropWhile(testCase.seq, testCase.pred)))
		})
	}
}

func TestSum(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		want int
	}{
		"sum of empty sequence is zero value": {
			seq:  Empty[int](),
			want: 0,
		},
		"sum of sequence is the sum of its elements": {
			seq:  FromValues(1, 2, 3),
			want: 6,
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, testCase.want, Sum(testCase.seq))
		})
	}
}

func TestSums(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			want: Empty[int](),
		},
		"singleton seq": {
			seq:  FromValues(42),
			want: FromValues(42),
		},
		"multi-element seq": {
			seq:  FromValues(1, 2, 3, 4),
			want: FromValues(1, 3, 6, 10),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, ToSlice(testCase.want), ToSlice(Sums(testCase.seq)))
		})
	}
}

func TestTake(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		n    int
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			n:    42,
			want: Empty[int](),
		},
		"take 3": {
			seq:  FromValues(1, 2, 3, 4),
			n:    3,
			want: FromValues(1, 2, 3),
		},
		"take more": {
			seq:  FromValues(1, 2, 3, 4),
			n:    42,
			want: FromValues(1, 2, 3, 4),
		},
		"take none": {
			seq:  FromValues(1, 2, 3, 4),
			n:    0,
			want: Empty[int](),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Take(testCase.seq, testCase.n)))
		})
	}
	t.Run("takes only n", func(t *testing.T) {
		require.Equal(t, []int{0, 1, 2}, ToSlice(Take(Inspect(Count(0, 1), func(idx int) {
			if idx == 3 {
				require.FailNow(t, "took more than required")
			}
		}), 3)))
	})
}

func TestTakeWhile(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		pred func(int) bool
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			pred: func(i int) bool { return true },
			want: Empty[int](),
		},
		"less than 3": {
			seq:  FromValues(1, 2, 3, 4, 3, 2, 1),
			pred: func(i int) bool { return i < 3 },
			want: FromValues(1, 2),
		},
		"take all": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return true },
			want: FromValues(1, 2, 3, 4),
		},
		"take none": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return false },
			want: Empty[int](),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(TakeWhile(testCase.seq, testCase.pred)))
		})
	}
}

func TestZipMany(t *testing.T) {
	testCases := map[string]struct {
		seqs []Seq[int]
		want Seq[[]int]
	}{
		"no seqs": {
			seqs: []Seq[int]{},
			want: Empty[[]int](),
		},
		"single seq": {
			seqs: []Seq[int]{
				FromValues(1, 2, 3, 4),
			},
			want: FromValues([]int{1}, []int{2}, []int{3}, []int{4}),
		},
		"seqs of same size": {
			seqs: []Seq[int]{
				FromValues(1, 2, 3, 4),
				FromValues(5, 6, 7, 8),
			},
			want: FromValues([]int{1, 5}, []int{2, 6}, []int{3, 7}, []int{4, 8}),
		},
		"seqs of different size": {
			seqs: []Seq[int]{
				FromValues(1, 2, 3),
				FromValues(4, 5, 6, 7),
				FromValues(8, 9),
			},
			want: FromValues([]int{1, 4, 8}, []int{2, 5, 9}),
		},
		"empty seq": {
			seqs: []Seq[int]{
				FromValues(1, 2, 3),
				FromValues(4, 5, 6, 7),
				Empty[int](),
			},
			want: Empty[[]int](),
		},
		"infinite seq": {
			seqs: []Seq[int]{
				FromValues(1, 2, 3),
				FromValues(4, 5, 6, 7),
				Repeat(0),
			},
			want: FromValues([]int{1, 4, 0}, []int{2, 5, 0}, []int{3, 6, 0}),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, ToSlice(testCase.want), ToSlice(ZipMany(testCase.seqs...)))
		})
	}
}

func TestZipWith(t *testing.T) {
	testCases := map[string]struct {
		seq1 Seq[int]
		seq2 Seq[int]
		want Seq[int]
	}{
		"empty seqs": {
			seq1: Empty[int](),
			seq2: Empty[int](),
			want: Empty[int](),
		},
		"seqs of same size": {
			seq1: FromValues(1, 2, 3),
			seq2: FromValues(4, 5, 6),
			want: FromValues(5, 7, 9),
		},
		"seqs of different size": {
			seq1: FromValues(1, 2, 3),
			seq2: FromValues(4, 5, 6, 7),
			want: FromValues(5, 7, 9),
		},
		"empty seq": {
			seq1: FromValues(1, 2, 3),
			seq2: Empty[int](),
			want: Empty[int](),
		},
		"infinite seq": {
			seq1: FromValues(1, 2, 3),
			seq2: Repeat(0),
			want: FromValues(1, 2, 3),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, ToSlice(testCase.want), ToSlice(ZipWith(testCase.seq1, testCase.seq2, add)))
		})
	}
}

func hideCard[Item any](seq Seq[Item]) Seq[Item] {
	return FromIter(ToIter(seq))
}

func add[Value Summable](a, b Value) Value {
	return a + b
}

type comparableInfiniteSeq[Item any] struct {
	item Item
	infiniteMark
}

func (seq comparableInfiniteSeq[Item]) ForEachWhile(yield func(Item) bool) {
	Repeat(seq.item).ForEachWhile(yield)
}
