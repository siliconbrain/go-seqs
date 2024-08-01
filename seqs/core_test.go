package seqs

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestAll(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		pred func(int) bool
		want bool
	}{
		"empty seq": {
			seq: Empty[int](),
			pred: func(i int) bool {
				return i < 42
			},
			want: true,
		},
		"seq without matching element": {
			seq: FromValues(21, 42, 84),
			pred: func(i int) bool {
				return i%5 == 0
			},
			want: false,
		},
		"seq with matching elements": {
			seq: FromValues(21, 42, 84),
			pred: func(i int) bool {
				return i > 30
			},
			want: false,
		},
		"seq of matching element": {
			seq: FromValues(21, 42, 84),
			pred: func(i int) bool {
				return i%3 == 0
			},
			want: true,
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, testCase.want, All(testCase.seq, testCase.pred))
		})
	}
}

func TestAny(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		pred func(int) bool
		want bool
	}{
		"empty seq": {
			seq: Empty[int](),
			pred: func(i int) bool {
				return i < 42
			},
			want: false,
		},
		"seq without matching element": {
			seq: FromValues(21, 42, 84),
			pred: func(i int) bool {
				return i%5 == 0
			},
			want: false,
		},
		"seq with matching element": {
			seq: FromValues(21, 42, 84),
			pred: func(i int) bool {
				return 30 < i && i < 50
			},
			want: true,
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, testCase.want, Any(testCase.seq, testCase.pred))
		})
	}
}

func TestAppendTo(t *testing.T) {
	testCases := map[string]struct {
		seq   Seq[int]
		slice []int
		want  []int
	}{
		"empty sequence does not change the slice": {
			seq:   Empty[int](),
			slice: []int{1, 2, 3},
			want:  []int{1, 2, 3},
		},
		"all sequence elements are appended to the slice in order": {
			seq:   FromValues(4, 5, 6),
			slice: []int{1, 2, 3},
			want:  []int{1, 2, 3, 4, 5, 6},
		},
		"sequence can be appended to nil slice": {
			seq:   FromValues(1, 2, 3),
			slice: nil,
			want:  []int{1, 2, 3},
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, testCase.want, AppendTo(testCase.seq, testCase.slice))
		})
	}
}

func TestAsSeq(t *testing.T) {
	var _ Seq[int] = AsSeq(seqFunc[int](nil))
}

func TestConcat(t *testing.T) {
	testCases := map[string]struct {
		seqs []Seq[int]
		want Seq[int]
	}{
		"no seqs": {
			seqs: []Seq[int]{},
			want: Empty[int](),
		},
		"one seq": {
			seqs: []Seq[int]{
				FromValues(1, 2, 3, 4),
			},
			want: FromValues(1, 2, 3, 4),
		},
		"two seqs": {
			seqs: []Seq[int]{
				FromValues(1, 2),
				FromValues(3, 4),
			},
			want: FromValues(1, 2, 3, 4),
		},
		"more seqs": {
			seqs: []Seq[int]{
				FromValues(1),
				FromValues(2),
				FromValues(3),
				FromValues(4),
			},
			want: FromValues(1, 2, 3, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Concat(testCase.seqs...)))
		})
	}
	t.Run("break early", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(1, 2, 2, 3, 3, 3)), ToSlice(Take(Concat(RepeatN(1, 1), RepeatN(2, 2), RepeatN(3, 3), RepeatN(4, 4)), 6)))
	})
	t.Run("with infinite seq", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(1, 2, 3, 4, 4, 4)), ToSlice(Take(Concat(FromValues(1, 2, 3), Repeat(4)), 6)))
	})
}

func TestCount(t *testing.T) {
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
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, testCase.want, Count(testCase.seq))
		})
	}
}

func TestCycle(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		take int
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			take: 10,
			want: Empty[int](),
		},
		"one element": {
			seq:  FromValues(1),
			take: 4,
			want: RepeatN(1, 4),
		},
		"more elements": {
			seq:  FromValues(1, 2, 3, 4),
			take: 10,
			want: FromValues(1, 2, 3, 4, 1, 2, 3, 4, 1, 2),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Take(Cycle(testCase.seq), testCase.take)))
		})
	}
}

func TestEmpty(t *testing.T) {
	require.Empty(t, ToSlice(Empty[int]()))
	require.Implements(t, (*Lener)(nil), Empty[int]())
	require.Zero(t, Empty[int]().(Lener).Len())
}

func TestEnumerate(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		want Seq[Pair[int, int]]
	}{
		"empty seq": {
			seq:  Empty[int](),
			want: Empty[Pair[int, int]](),
		},
		"some seq": {
			seq:  FromValues(11, 22, 33, 44, 55),
			want: FromValues(pairOf(0, 11), pairOf(1, 22), pairOf(2, 33), pairOf(3, 44), pairOf(4, 55)),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, ToSlice(testCase.want), ToSlice(Enumerate(testCase.seq)))
		})
	}
}

func TestFilter(t *testing.T) {
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
		"every other": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return i%2 == 0 },
			want: FromValues(2, 4),
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
			require.Equal(t, ToSlice(testCase.want), ToSlice(Filter(testCase.seq, testCase.pred)))
		})
	}
}

func TestFilterMap(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		fn   func(int) (int, bool)
		want Seq[int]
	}{
		"empty seq": {
			seq:  Empty[int](),
			fn:   func(i int) (int, bool) { return i + 1, true },
			want: Empty[int](),
		},
		"every other": {
			seq:  FromValues(1, 2, 3, 4),
			fn:   func(i int) (int, bool) { return i * 2, i%2 == 0 },
			want: FromValues(4, 8),
		},
		"take all": {
			seq:  FromValues(1, 2, 3, 4),
			fn:   func(i int) (int, bool) { return i + 1, true },
			want: FromValues(2, 3, 4, 5),
		},
		"take none": {
			seq:  FromValues(1, 2, 3, 4),
			fn:   func(i int) (int, bool) { return i * 0, false },
			want: Empty[int](),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(FilterMap(testCase.seq, testCase.fn)))
		})
	}
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
			seq:          GenerateWithIndex(func(idx int) int { return idx + 2 }),
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

func TestForEachWithIndex(t *testing.T) {
	ForEachWithIndex(FromValues(1, 2, 3, 4), func(i int, e int) {
		require.Equal(t, i, e-1)
	})
}

func TestForEachUntilWithIndex(t *testing.T) {
	var vals []int
	ForEachUntilWithIndex(FromValues(1, 2, 3, 4), func(i int, e int) bool {
		require.Equal(t, i, e-1)
		vals = append(vals, e)
		return len(vals) > 2
	})
	require.Equal(t, []int{1, 2, 3}, vals)
}

func TestForEachWhile(t *testing.T) {
	var vals []int
	ForEachWhile(FromValues(1, 2, 3, 4), func(i int) bool {
		vals = append(vals, i)
		return len(vals) < 3
	})
	require.Equal(t, []int{1, 2, 3}, vals)
}

func TestForEachWhileWithIndex(t *testing.T) {
	var vals []int
	ForEachWhileWithIndex(FromValues(1, 2, 3, 4), func(i int, e int) bool {
		require.Equal(t, i, e-1)
		vals = append(vals, e)
		return len(vals) < 3
	})
	require.Equal(t, []int{1, 2, 3}, vals)
}

func TestFromValue(t *testing.T) {
	seq := FromValue(42)
	require.Equal(t, []int{42}, ToSlice(seq))
	require.Equal(t, 1, seq.(Lener).Len())
}

func TestFromSlicePtrs(t *testing.T) {
	vals := []int{1, 2, 3, 4}
	seq := FromSlicePtrs(vals)
	ForEach(seq, func(i *int) { *i = *i * 2 })
	assert.Equal(t, []int{2, 4, 6, 8}, vals)

	// cover Len() method
	lener, ok := seq.(Lener)
	require.True(t, ok)
	assert.Equal(t, len(vals), lener.Len())

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

func TestGenerateWithIndex(t *testing.T) {
	testCases := map[string]struct {
		fn   func(idx int) int
		want []int
	}{
		"count by 2": {
			fn: func(idx int) int {
				return idx * 2
			},
			want: []int{0, 2, 4, 6},
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, testCase.want, ToSlice(Take(GenerateWithIndex(testCase.fn), len(testCase.want))))
		})
	}
}

func TestIntersperse(t *testing.T) {
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

func TestPartialSums(t *testing.T) {
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
			assert.Equal(t, ToSlice(testCase.want), ToSlice(PartialSums(testCase.seq)))
		})
	}
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
			want: GenerateWithIndex(func(idx int) int { return idx + 1 }),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, ToSlice(Take(testCase.want, maxSamples)), ToSlice(Take(Reductions(testCase.seq, testCase.op), maxSamples)))
		})
	}
}

func TestReject(t *testing.T) {
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
		"every other": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return i%2 == 0 },
			want: FromValues(1, 3),
		},
		"take all": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return false },
			want: FromValues(1, 2, 3, 4),
		},
		"take none": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(i int) bool { return true },
			want: Empty[int](),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(Reject(testCase.seq, testCase.pred)))
		})
	}
}

func TestRejectWithIndex(t *testing.T) {
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
			want: FromValues(2, 4),
		},
		"take all": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(idx int, itm int) bool { return false },
			want: FromValues(1, 2, 3, 4),
		},
		"take none": {
			seq:  FromValues(1, 2, 3, 4),
			pred: func(idx int, itm int) bool { return true },
			want: Empty[int](),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(RejectWithIndex(testCase.seq, testCase.pred)))
		})
	}
}

func TestRepeat(t *testing.T) {
	require.Equal(t, ToSlice(RepeatN(42, 6)), ToSlice(Take(Repeat(42), 6)))
}

func TestRoundRobin(t *testing.T) {
	testCases := map[string]struct {
		seqs []Seq[int]
		want Seq[int]
	}{
		"no seqs": {
			seqs: []Seq[int]{},
			want: Empty[int](),
		},
		"one seq": {
			seqs: []Seq[int]{
				FromValues(1, 2, 3, 4),
			},
			want: FromValues(1, 2, 3, 4),
		},
		"two seqs": {
			seqs: []Seq[int]{
				FromValues(1, 3),
				FromValues(2, 4),
			},
			want: FromValues(1, 2, 3, 4),
		},
		"more seqs": {
			seqs: []Seq[int]{
				FromValues(1),
				FromValues(2, 2),
				FromValues(3, 3, 3),
				FromValues(4, 4, 4, 4),
			},
			want: FromValues(1, 2, 3, 4, 2, 3, 4, 3, 4, 4),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(RoundRobin(testCase.seqs...)))
		})
	}
	t.Run("break early", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(1, 2, 3, 4, 2, 3)), ToSlice(Take(RoundRobin(RepeatN(1, 1), RepeatN(2, 2), RepeatN(3, 3), RepeatN(4, 4)), 6)))
	})
	t.Run("break even earlier", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(1, 2, 3)), ToSlice(Take(RoundRobin(RepeatN(1, 1), RepeatN(2, 2), RepeatN(3, 3), RepeatN(4, 4)), 3)))
	})
	t.Run("with infinite seq", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(1, 2, 42, 3, 4, 42, 42, 42)), ToSlice(Take(RoundRobin(FromValues(1, 3), FromValues(2, 4), Repeat(42)), 8)))
	})
}

func TestSeededReduce(t *testing.T) {
	t.Run("empty seq", func(t *testing.T) {
		require.Equal(t, 42, SeededReduce(Empty[int](), 42, func(a int, e int) int { return a + e }))
	})
	t.Run("offset sum", func(t *testing.T) {
		require.Equal(t, 20, SeededReduce(FromValues(1, 2, 3, 4), 10, func(a int, e int) int { return a + e }))
	})
}

func TestSeededReductions(t *testing.T) {
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
			want: GenerateWithIndex(func(idx int) int { return idx }),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, ToSlice(Take(testCase.want, maxSamples)), ToSlice(Take(SeededReductions(testCase.seq, testCase.seed, testCase.op), maxSamples)))
		})
	}

	t.Run("break early", func(t *testing.T) {
		assert.Equal(t, []int{42}, ToSlice(Take(SeededReductions(Repeat(1), 42, add), 1)))
	})
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
			require.Equal(t, ToSlice(testCase.want), ToSlice(Skip(testCase.seq, testCase.n)))
		})
	}
	t.Run("infinite seq", func(t *testing.T) {
		require.Equal(t, ToSlice(FromValues(3, 4, 1, 2, 3, 4, 1, 2)), ToSlice(Take(Skip(Cycle(FromValues(1, 2, 3, 4)), 2), 8)))
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
			require.Equal(t, ToSlice(testCase.want), ToSlice(SkipWhile(testCase.seq, testCase.pred)))
		})
	}
}

func TestSlidingWindow(t *testing.T) {
	t.Run("zero count", func(t *testing.T) {
		require.Panics(t, func() {
			SlidingWindow(FromValues(1, 2, 3, 4), 0, 1)
		})
	})
	t.Run("zero skip", func(t *testing.T) {
		require.Panics(t, func() {
			SlidingWindow(FromValues(1, 2, 3, 4), 1, 0)
		})
	})
	testCases := map[string]struct {
		seq   Seq[int]
		count int
		skip  int
		want  Seq[[]int]
	}{
		"empty seq": {
			seq:   Empty[int](),
			count: 1,
			skip:  1,
			want:  Empty[[]int](),
		},
		"each element by itself": {
			seq:   FromValues(1, 2, 3, 4),
			count: 1,
			skip:  1,
			want:  FromValues([]int{1}, []int{2}, []int{3}, []int{4}),
		},
		"overlapping pairs": {
			seq:   FromValues(1, 2, 3, 4),
			count: 2,
			skip:  1,
			want:  FromValues([]int{1, 2}, []int{2, 3}, []int{3, 4}),
		},
		"adjecent pairs": {
			seq:   FromValues(1, 2, 3, 4),
			count: 2,
			skip:  2,
			want:  FromValues([]int{1, 2}, []int{3, 4}),
		},
		"disjunct pairs": {
			seq:   FromValues(1, 2, 3, 4, 5, 6, 7, 8),
			count: 2,
			skip:  4,
			want:  FromValues([]int{1, 2}, []int{5, 6}),
		},
		"overlapping triplets": {
			seq:   FromValues(1, 2, 3, 4),
			count: 3,
			skip:  1,
			want:  FromValues([]int{1, 2, 3}, []int{2, 3, 4}),
		},
		"adjecent triplets": {
			seq:   FromValues(1, 2, 3, 4, 5, 6, 7),
			count: 3,
			skip:  3,
			want:  FromValues([]int{1, 2, 3}, []int{4, 5, 6}),
		},
		"disjunct triplets": {
			seq:   FromValues(1, 2, 3, 4, 5, 6, 7),
			count: 3,
			skip:  4,
			want:  FromValues([]int{1, 2, 3}, []int{5, 6, 7}),
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, ToSlice(testCase.want), ToSlice(SlidingWindow(testCase.seq, testCase.count, testCase.skip)))
		})
	}
	t.Run("break early", func(t *testing.T) {
		require.Equal(t, [][]int{{1, 2}, {2, 3}}, ToSlice(Take(SlidingWindow(FromValues(1, 2, 3, 4, 5, 6), 2, 1), 2)))
	})
	t.Run("with infinite seq", func(t *testing.T) {
		require.Equal(t, [][]int{{1, 1}, {1, 1}}, ToSlice(Take(SlidingWindow(Repeat(1), 2, 1), 2)))
	})
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
		require.Equal(t, []int{0, 1, 2}, ToSlice(Take(GenerateWithIndex(func(idx int) int {
			if idx == 3 {
				require.FailNow(t, "took more than required")
			}
			return idx
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

func TestToSet(t *testing.T) {
	testCases := map[string]struct {
		seq  Seq[int]
		want map[int]bool
	}{
		"empty seq": {
			seq:  Empty[int](),
			want: map[int]bool{},
		},
		"unique values": {
			seq:  FromValues(1, 2, 3, 4),
			want: map[int]bool{1: true, 2: true, 3: true, 4: true},
		},
		"repeating values": {
			seq:  FromValues(1, 2, 3, 4, 3, 2, 1),
			want: map[int]bool{1: true, 2: true, 3: true, 4: true},
		},
		"seq without len": {
			seq: SeqFunc(func(f func(int) bool) {
				if f(1) {
					return
				}
				if f(2) {
					return
				}
				if f(1) {
					return
				}
				if f(2) {
					return
				}
			}),
			want: map[int]bool{1: true, 2: true},
		},
	}
	for name, testCase := range testCases {
		testCase := testCase
		t.Run(name, func(t *testing.T) {
			require.Equal(t, testCase.want, ToSet(testCase.seq))
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
